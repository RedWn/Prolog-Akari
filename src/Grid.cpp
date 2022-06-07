#include<Grid.h>
#include<SWI-cpp.h>
Grid::Grid(){}
Grid& Grid::instance(){
    static Grid grid;
    return grid;
}
void Grid::init(){
    PlTerm sizeX, sizeY;
    PlCall("size", PlTermv(sizeX, sizeY));
    horizontalTilesCount = sizeX;
    verticalTilesCount = sizeY;
    if(horizontalTilesCount>verticalTilesCount){
        tileSideSize = 2.0f / (float)horizontalTilesCount;
    }else{
        tileSideSize = 2.0f / (float)verticalTilesCount;
    }
    for (int i = 0; i < horizontalTilesCount;++i){
        for (int j = 0; j < verticalTilesCount;++j){
            tiles.push_back(Tile(i + 1, j + 1));
        }
    }
}