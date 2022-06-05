#include <SFML/Window.hpp>
#define WINDOW_HEIGHT 1024
#define WINDOW_WIDTH 1024
using namespace sf;
int main(int argc, char **argv)
{
    ContextSettings ctxSettings{};
    ctxSettings.antialiasingLevel = 4;
    ctxSettings.attributeFlags = ctxSettings.Core;
    ctxSettings.depthBits = 0;
    ctxSettings.majorVersion = 3;
    ctxSettings.minorVersion = 3;
    ctxSettings.sRgbCapable = false;
    ctxSettings.stencilBits = 0;
    Window window(VideoMode(WINDOW_HEIGHT, WINDOW_WIDTH), "Akari", 7U, ctxSettings);
    while (1)
    {
        Event event;
        while (window.pollEvent(event))
        {
            switch (event.type)
            {
            case Event::Closed:
                return 0;
                break;

            default:
                break;
            }
        }
        window.display();
    }
}