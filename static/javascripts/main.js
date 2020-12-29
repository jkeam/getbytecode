window.onReady = function(jQuery) {
  const application = Stimulus.Application.start();
  application.register('application', class extends Stimulus.Controller {
    static get targets() {
      return [
        'languageversion',
        'input',
        'output',
        'dissbutton'
      ]
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

      this.inputMirror = CodeMirror.fromTextArea(this.inputTarget, {lineNumbers:true});
      this.outputMirror = CodeMirror.fromTextArea(this.outputTarget, {lineNumbers:true, readOnly :true});
      this.previousKey = '';
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
      const doc = this.inputMirror.getDoc();

      if (this.previousKey !== key) {
        // set new example text
        doc.setValue(this.languageToSnippet[key]);
        // set new language
        this.inputMirror.setOption('mode', this.languageToMode[key]);
      }
      this.previousKey = key;
    }

    submitCode(e) {
      e.preventDefault();
      const rawText = this.inputMirror.getValue();
      if (!rawText) return;

      this.dissbuttonTarget.innerHTML = 'Running...';
      this.dissbuttonTarget.classList.add('disabled');

      const that = this;
      jQuery.ajax({
        type: 'POST',
        url: '/',
        dataType: 'text',
        contentType: 'application/json; charset=utf-8',
        data: JSON.stringify({"code":rawText}),
        complete: function(xhr, status) {
          that.dissbuttonTarget.innerHTML = 'Disassemble';
          that.dissbuttonTarget.classList.remove('disabled');
        },
        success: function(code) {
          that.outputMirror.setValue(code.replace(/<br>/g, "\n"));
        },
        error: function(xhr, status, errorThrown) {
          console.error('Error compiling');
        }
      });
    }
  });
}
