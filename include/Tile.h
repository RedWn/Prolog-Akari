#pragma once
class Tile{
    int x, y;
    int wallVal;

public:
    enum Type
    {
        LIGHT,
        WALL,
        NUM_WALL,
        VOID,
        LIT_VOID,
        NUM_TYPES
    };
    Tile(int x, int y);
    Type type;
    void draw();
};