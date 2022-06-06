#pragma once
#include<vector>
#include<Tile.h>
#define GRID Grid::instance()
class Grid{
    private:
        Grid();
        std::vector<Tile> tiles;
        int tileSideSize;
        int horizontalTilesCount, verticalTilesCount;

    public:
        static Grid &instance();
        void init();
        void update();
        void draw();
};