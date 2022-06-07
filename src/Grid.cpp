#include<Grid.h>
#include<SWI-cpp.h>
Grid::Grid(){}
Grid& Grid::instance(){
    static Grid grid;
    return grid;
}
void Grid::init(){
    PlTerm sizeX, sizeY;
    PlQuery query("size", PlTermv(sizeX, sizeY));
    query.next_solution();
    horizontalTilesCount = sizeX;
    verticalTilesCount = sizeY;
}