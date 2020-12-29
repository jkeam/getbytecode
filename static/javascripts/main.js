var inputMirror;
var outputMirror;

window.onReady = function() {
  const application = Stimulus.Application.start();
  application.register('application', class extends Stimulus.Controller {
    static get targets() {
      return [ 'languageversion' ]
    }

    static get values() {
      return { initiallanguageversion: String }
    }

    initialize() {
      this.languageToSnippet = {
        ruby: "puts 'hi'",
        python: "print('hi')",
        java: `public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello World!");
    }
}`,
        erlang: `-module(hello).
-export([hello_world/0]).

hello_world() -> io:fwrite("hello, world").
`
      };
      this.languageToMode = {
        ruby: 'text/x-ruby',
        python: 'text/x-python',
        java: 'text/x-java',
        erlang: 'text/x-erlang'
      };

      this.previousKey = this.keyFromLanguageVersion(this.initiallanguageversionValue);
      this.resetInput(this.initiallanguageversionValue);
    }

    choose(e) {
      e.preventDefault();
      this.resetInput(e.target.text);
    }

    keyFromLanguageVersion(languageVersion){
      return languageVersion.split(' ')[0].toLowerCase();
    }

    resetInput(chosen) {
      this.languageversionTarget.innerHTML = chosen;
      const key = this.keyFromLanguageVersion(chosen);
      const doc = inputMirror.getDoc();

      if (this.previousKey !== key) {
        // set new example text
        doc.setValue(this.languageToSnippet[key]);
        // set new language
        inputMirror.setOption('mode', this.languageToMode[key]);
      }
      this.previousKey = key;
    }
  });
}

function Codebytes() {
  this.submitCode = function() {
    $('#disassemble_button').attr('disabled', 'disabled');
    outputMirror.setValue("Running...");

    const rawText = inputMirror.getValue();
    if (!rawText) return;

    $.ajax({
      type: 'POST',
      url: '/',
      dataType: 'text',
      contentType: 'application/json; charset=utf-8',
      data: JSON.stringify({"code":rawText}),
      complete: function(xhr, status) {
        $('#disassemble_button').removeAttr('disabled');
      },
      success: function(code) {
        code = code.replace(/<br>/g, "\n");
        outputMirror.setValue(code);
      },
      error: function(xhr, status, errorThrown) {
        console.error('Error compiling');
      }
    });
  };

  this.onReady = function() {
    inputMirror = CodeMirror.fromTextArea($("#inputTextArea").get(0), {lineNumbers:true});
    outputMirror = CodeMirror.fromTextArea($("#outputTextArea").get(0), {lineNumbers:true, readOnly :true});
  };
}
window.codebytes = new Codebytes();
