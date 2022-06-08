#include <Grid.h>
#include <SWI-cpp.h>
#include <SFML/Graphics/Image.hpp>
#include<fstream>
using namespace std;
using namespace gl;
using namespace sf;
Grid::Grid() {}
Grid &Grid::instance()
{
    static Grid grid;
    return grid;
}
void Grid::init()
{
    glEnable(GL_PROGRAM_POINT_SIZE);
    PlTerm sizeX, sizeY;
    PlCall("size", PlTermv(sizeX, sizeY));
    horizontalTilesCount = sizeX;
    verticalTilesCount = sizeY;
    if (horizontalTilesCount > verticalTilesCount)
    {
        tileSideSize = 2.0f / (float)horizontalTilesCount;
    }
    else
    {
        tileSideSize = 2.0f / (float)verticalTilesCount;
    }
    for (int i = 0; i < horizontalTilesCount; ++i)
    {
        for (int j = 0; j < verticalTilesCount; ++j)
        {
            tiles.push_back(Tile(i + 1, j + 1));
        }
    }

    // Load textures
    Image images[Tile::NUM_TYPES];
    images[Tile::LIGHT].loadFromFile("Assets/Light.png");   
    images[Tile::WALL].loadFromFile("Assets/Wall.png");     
    images[Tile::WALL_0].loadFromFile("Assets/Wall_0.png"); 
    images[Tile::WALL_1].loadFromFile("Assets/Wall_1.png"); 
    images[Tile::WALL_2].loadFromFile("Assets/Wall_2.png"); 
    images[Tile::WALL_3].loadFromFile("Assets/Wall_3.png"); 
    images[Tile::WALL_4].loadFromFile("Assets/Wall_4.png"); 
    images[Tile::VOID].loadFromFile("Assets/Void.png");     
    images[Tile::LIT_VOID].loadFromFile("Assets/Lit_Void.png");
    for (int i = 0; i < Tile::NUM_TYPES; ++i)
    {
        glGenTextures(1, &textures[i]);
        glBindTexture(GL_TEXTURE_2D, textures[i]);
        glTextureParameteri(textures[i], GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTextureParameteri(textures[i], GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, images[i].getSize().x, images[i].getSize().y, 0,
                     GL_RGBA, GL_UNSIGNED_BYTE, images[i].getPixelsPtr());
    }
    glBindTexture(GL_TEXTURE_2D, 0);
    shaderProgram = loadShaderProgram("Assets/VertexShader.vs", "Assets/FragmentShader.fs");
    assert(glGetError() == 0);
    glUseProgram(shaderProgram);
    uniformsLocations[Uniforms::X] = glGetUniformLocation(shaderProgram, "x");
    uniformsLocations[Uniforms::Y] = glGetUniformLocation(shaderProgram, "y");
    uniformsLocations[Uniforms::POINT_SIZE] = glGetUniformLocation(shaderProgram, "size");
    uniformsLocations[Uniforms::SIZE_FACTOR] = glGetUniformLocation(shaderProgram, "sizeFactor");
    glUniform1f(uniformsLocations[Uniforms::SIZE_FACTOR], (float)WINDOW_SIDE*0.5f);
    glUniform1f(uniformsLocations[Uniforms::POINT_SIZE], tileSideSize);
    assert(glGetError() == 0);
}
void Grid::draw(){
    glClear(GL_COLOR_BUFFER_BIT);
    for (Tile tile : tiles)
    {
        tile.draw();
    }
}
void Grid::solve(){
    PlCall("solve");
    for(Tile &tile:tiles){
        tile.update();
    }
}
GLuint Grid::loadShaderProgram(const char *vertexShaderPath, const char *fragmentShaderPath)
{
    GLuint program;
    vector<char> vertexShaderCode = readBinFile(vertexShaderPath);
    vector<char> fragmentShaderCode = readBinFile(fragmentShaderPath);
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    GLint vertexShaderSize = vertexShaderCode.size();
    GLint fragmentShaderSize = fragmentShaderCode.size();
    char *pVertexShaderCode = vertexShaderCode.data();
    char *pFragmentShaderCode = fragmentShaderCode.data();
    glShaderSource(vertexShader, 1, &pVertexShaderCode, &vertexShaderSize);
    glShaderSource(fragmentShader, 1, &pFragmentShaderCode, &fragmentShaderSize);
    glCompileShader(vertexShader);
    glCompileShader(fragmentShader);
#ifdef DEBUG
    GLint infoLogLen;
    glGetShaderiv(vertexShader, GL_INFO_LOG_LENGTH, &infoLogLen);
    vector<char> infoLog(infoLogLen);
    glGetShaderInfoLog(vertexShader, infoLog.size(), nullptr, infoLog.data());
    infoLog.push_back('\0');
    printf(infoLog.data());
    infoLog.resize(0);
    glGetShaderiv(fragmentShader, GL_INFO_LOG_LENGTH, &infoLogLen);
    infoLog.resize(infoLogLen);
    glGetShaderInfoLog(fragmentShader, infoLog.size(), nullptr, infoLog.data());
    infoLog.push_back('\0');
    printf(infoLog.data());
    infoLog.resize(0);
#endif
    program = glCreateProgram();
    glAttachShader(program, vertexShader);
    glAttachShader(program, fragmentShader);
    glLinkProgram(program);
#ifdef DEBUG
    glGetProgramiv(program, GL_INFO_LOG_LENGTH, &infoLogLen);
    infoLog.resize(infoLogLen);
    glGetProgramInfoLog(program, infoLogLen, nullptr, infoLog.data());
    infoLog.push_back('\0');
    printf(infoLog.data());
#endif
    glDetachShader(program, vertexShader);
    glDetachShader(program, fragmentShader);
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    assert(glGetError() == 0);
    return program;
}

vector<char> Grid::readBinFile(const char* path){
    vector<char> data;
    fstream fileStream(path, ios::binary | ios::ate | ios::in);
    if(!fileStream.is_open()){
        printf("Failed to open file at %s", path);
        assert(0);
    }
    data.resize(fileStream.tellg());
    fileStream.seekg(0, ios::beg);
    fileStream.read(data.data(), data.size());
    fileStream.flush();
    fileStream.close();
    return data;
}