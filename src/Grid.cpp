#include <Grid.h>
#include <SWI-cpp.h>
#include <SFML/Graphics/Image.hpp>
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
    assert(glGetError() == 0);
}