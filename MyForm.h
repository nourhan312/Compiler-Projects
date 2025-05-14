#pragma once
#include "TokenizerParser.h" 
#include <msclr/marshal_cppstd.h> 

namespace TinyLangGUI {

    using namespace System;
    using namespace System::Windows::Forms;
    using namespace msclr::interop; 

    public ref class Form1 : public Form
    {
    public:
        Form1()
        {
            InitializeComponent();
        }

    protected:
        ~Form1()
        {
            if (components)
                delete components;
        }

    private:
        System::ComponentModel::Container^ components;
        TextBox^ textBoxInput;
        Button^ buttonAnalyze;
        TextBox^ textBoxOutput;

        void InitializeComponent(void)
        {
            this->textBoxInput = (gcnew System::Windows::Forms::TextBox());
            this->buttonAnalyze = (gcnew System::Windows::Forms::Button());
            this->textBoxOutput = (gcnew System::Windows::Forms::TextBox());
            this->SuspendLayout();
            // 
            // textBoxInput
            // 
            this->textBoxInput->Dock = System::Windows::Forms::DockStyle::Top;
            this->textBoxInput->Font = (gcnew System::Drawing::Font(L"Consolas", 10));
            this->textBoxInput->Location = System::Drawing::Point(0, 0);
            this->textBoxInput->Margin = System::Windows::Forms::Padding(4, 4, 4, 4);
            this->textBoxInput->Multiline = true;
            this->textBoxInput->Name = L"textBoxInput";
            this->textBoxInput->Size = System::Drawing::Size(1032, 260);
            this->textBoxInput->TabIndex = 2;
            // 
            // buttonAnalyze
            // 
            this->buttonAnalyze->Dock = System::Windows::Forms::DockStyle::Top;
            this->buttonAnalyze->Location = System::Drawing::Point(0, 260);
            this->buttonAnalyze->Margin = System::Windows::Forms::Padding(4, 4, 4, 4);
            this->buttonAnalyze->Name = L"buttonAnalyze";
            this->buttonAnalyze->Size = System::Drawing::Size(1032, 28);
            this->buttonAnalyze->TabIndex = 1;
            this->buttonAnalyze->Text = L"Analyze";
            this->buttonAnalyze->Click += gcnew System::EventHandler(this, &Form1::buttonAnalyze_Click);
            // 
            // textBoxOutput
            // 
            this->textBoxOutput->Dock = System::Windows::Forms::DockStyle::Fill;
            this->textBoxOutput->Font = (gcnew System::Drawing::Font(L"Consolas", 10));
            this->textBoxOutput->Location = System::Drawing::Point(0, 288);
            this->textBoxOutput->Margin = System::Windows::Forms::Padding(4, 4, 4, 4);
            this->textBoxOutput->Multiline = true;
            this->textBoxOutput->Name = L"textBoxOutput";
            this->textBoxOutput->ReadOnly = true;
            this->textBoxOutput->ScrollBars = System::Windows::Forms::ScrollBars::Both;
            this->textBoxOutput->Size = System::Drawing::Size(1032, 333);
            this->textBoxOutput->TabIndex = 0;
            // 
            // Form1
            // 
            this->AutoScaleDimensions = System::Drawing::SizeF(8, 16);
            this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
            this->ClientSize = System::Drawing::Size(1032, 621);
            this->Controls->Add(this->textBoxOutput);
            this->Controls->Add(this->buttonAnalyze);
            this->Controls->Add(this->textBoxInput);
            this->Margin = System::Windows::Forms::Padding(4, 4, 4, 4);
            this->Name = L"Form1";
            this->Text = L"Tiny Language Tokenizer & Parser";
            this->ResumeLayout(false);
            this->PerformLayout();

        }

        System::Void buttonAnalyze_Click(System::Object^ sender, System::EventArgs^ e)
        {
            // Marshal System::String to std::string for native functions
            std::string code = marshal_as<std::string>(textBoxInput->Text);

            // Call native tokenizer
            std::vector<Token> tokens = tokenize(code);

            // Call native parser
            ParseResult result; // Create a ParseResult object
            parseTokens(tokens, result); // Pass tokens and result by reference

            // Build output string using System::String from the results
            System::String^ outputText = ""; 

            outputText += "--- TOKENS ---\r\n";
            for (size_t i = 0; i < tokens.size(); ++i) {
                const auto& t = tokens[i];
                // Marshal std::string token value and type to System::String
                outputText += System::String::Format("Line {0}: {1} '{2}'\r\n",
                    t.line,
                    marshal_as<System::String^>(tokenTypeToString(t.type)),
                    marshal_as<System::String^>(t.value));
            }


            // Display parsing steps
            /*outputText += "\r\n--- PARSING STEPS ---\r\n";
            for (size_t i = 0; i < result.steps.size(); ++i) {
                outputText += marshal_as<System::String^>(result.steps[i]) + "\r\n";
            }*/


            // Display errors
            if (!result.errors.empty()) {
                outputText += "\r\n--- SYNTAX ERRORS FOUND (" + result.errors.size() + ") ---\r\n";
                for (size_t i = 0; i < result.errors.size(); ++i) {
                    const auto& error = result.errors[i]; // Get reference using index
                    // Manually format error details for System::String output
                    outputText += "-\r\n";
                    outputText += "  Type:      " + marshal_as<System::String^>(errorTypeToString(error.type)) + " (Line " + error.line + ")\r\n";
                    outputText += "  Message:   " + marshal_as<System::String^>(error.message) + "\r\n";
                    if (!error.foundValue.empty() || !error.foundType.empty()) {
                        outputText += "  Found:     '" + marshal_as<System::String^>(error.foundValue) + "' (" + marshal_as<System::String^>(error.foundType) + ")\r\n";
                    }
                    if (!error.expectedDesc.empty()) {
                        outputText += "  Expected:  " + marshal_as<System::String^>(error.expectedDesc) + "\r\n";
                    }
                    if (!error.suggestion.empty()) {
                        outputText += "  Suggestion: " + marshal_as<System::String^>(error.suggestion) + "\r\n";
                    }
                }
                outputText += "---------------------------\r\n";
            }
            else {
                outputText += "\r\n--- No Syntax Errors Reported ---\r\n";
            }

            textBoxOutput->Text = outputText;
        }
    };
}
