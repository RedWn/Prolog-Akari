#pragma once
#include<vector>
#include<Tile.h>
#include<glbinding/gl33/gl.h>
#define GRID Grid::instance()
class Grid{
    private:
        Grid();
        std::vector<Tile> tiles;
        gl::GLuint textures[Tile::NUM_TYPES];
        float tileSideSize;
        int horizontalTilesCount, verticalTilesCount;

    public:
        static Grid &instance();
        void init();
        void update();
        void draw();
};