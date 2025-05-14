#include "MyForm.h"  
#include <Windows.h> 

using namespace TinyLangGUI; 

[STAThread] 
int main() {
    Application::EnableVisualStyles();
    Application::SetCompatibleTextRenderingDefault(false);

    Application::Run(gcnew Form1());

    return 0;
}
