#include <Tile.h>
#include <SWI-cpp.h>
Tile::Tile(int x, int y) : x(x), y(y)
{
    char buffer[128];
    PlTerm value;
    if (PlCall("wall_num", PlTermv(PlTerm(long(x)), PlTerm(long(y)), value)))
    {
        type = NUM_WALL;
        wallVal = value;
        return;
    }
    if (PlCall("wall", PlTermv(PlTerm(long(x)), PlTerm(long(y)))))
    {
        type = WALL;
        return;
    }

    if (sprintf(buffer, "light(cell(%d,%d))", x, y) &&
        PlCall(buffer))
    {
        type = LIGHT;
        return;
    }
    if (sprintf(buffer, "is_cell_lighted(cell(%d, %d))", x, y) &&
        PlCall(buffer))
    {
        type = LIT_VOID;
        return;
    }
    type = VOID;
}