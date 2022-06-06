#include<Grid.h>
Grid::Grid(){}
Grid& Grid::instance(){
    static Grid grid;
    return grid;
}
