#pragma once
#include<vector>
#include<Tile.h>
#include<glbinding/gl33/gl.h>
#define WINDOW_SIDE 650
#define GRID Grid::instance()
enum Uniforms
{
    X,
    Y,
    POINT_SIZE,
    SIZE_FACTOR,
    UNIFORM_NUM
};
class Grid
{
private:
    Grid();
    std::vector<char> readBinFile(const char *path);
    gl::GLuint loadShaderProgram(const char *vertexShaderPath, const char *fragmentShaderPath);
    std::vector<Tile> tiles;
    gl::GLuint shaderProgram;
    float tileSideSize;
    int horizontalTilesCount, verticalTilesCount;

public:
    static Grid &instance();
    void init();
    void solve();
    void draw();
    gl::GLuint textures[Tile::NUM_TYPES];
    gl::GLint uniformsLocations[Uniforms::UNIFORM_NUM];
};