#pragma once
#include<assert.h>
class Tile{
    int x, y;
    int wallVal;

public:
    enum Type
    {
        LIGHT,
        WALL,
        WALL_0,
        WALL_1,
        WALL_2,
        WALL_3,
        WALL_4,
        VOID,
        LIT_VOID,
        NUM_TYPES
    };
    Tile(int x, int y);
    Type type;
    void draw();
};