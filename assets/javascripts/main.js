window.onReady = function(jQuery, endpoints) {
  const application = Stimulus.Application.start();
  application.register('application', class extends Stimulus.Controller {
    static get targets() {
      return [
        'languageVersion',
        'input',
        'output',
        'dissbutton'
      ];
    }

    static get values() {
      return {
        initialLanguageVersion: String,
        submitButtonSubmittingText: String,
        submitButtonOriginalText: String
      };
    }

    static get classes() {
      return ['submitButtonSubmitting'];
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

hello_world() -> io:fwrite("hello, world").`
      };
      this.languageToMode = {
        ruby: 'text/x-ruby',
        python: 'text/x-python',
        java: 'text/x-java',
        erlang: 'text/x-erlang'
      };

      this.valid = false;
      this.selectedLanguage = '';
      this.onInputChangeCallback = this.onInputChange.bind(this);
    }

    connect() {
      this.inputMirror = CodeMirror.fromTextArea(this.inputTarget, {lineNumbers:true});
      this.outputMirror = CodeMirror.fromTextArea(this.outputTarget, {lineNumbers:true, readOnly :true});

      this.resetInput(this.initialLanguageVersionValue);
      this.inputMirror.on('change', this.onInputChangeCallback);
    }

    disconnect() {
      this.inputMirror.off('change', this.onInputChangeCallback);
    }

    onInputChange(editor) {
      this.valid = editor.getValue().trim() !== '';
      this.toggleSubmitButton(this.valid);
    }

    choose(e) {
      e.preventDefault();
      this.resetInput(e.target.text);
    }

    keyFromLanguageVersion(languageVersion){
      return (languageVersion || '').split(' ')[0].toLowerCase();
    }

    resetInput(chosen) {
      this.selectedLanguage = chosen;
      this.languageVersionTarget.innerHTML = chosen;
      const key = this.keyFromLanguageVersion(chosen);

      // set new language mode
      this.inputMirror.setOption('mode', this.languageToMode[key]);
      // reset input and output text
      this.inputMirror.getDoc().setValue(this.languageToSnippet[key]);
      this.outputMirror.getDoc().setValue('');
      this.valid = true;
    }

    toggleSubmitButton(enabled) {
      const submittingClass = this.submitButtonSubmittingClass;
      if (enabled) {
        this.dissbuttonTarget.classList.remove(submittingClass);
      } else {
        this.dissbuttonTarget.classList.add(submittingClass);
      }
    }

    submitCode(e) {
      e.preventDefault();
      if (!this.valid) return;

      const code = this.inputMirror.getValue();
      this.toggleSubmitButton(false);
      this.dissbuttonTarget.innerHTML = this.submitButtonSubmittingTextValue;
      const selected = endpoints.find(e => e.name === this.selectedLanguage);
      const url = selected.url || '/';

      const that = this;
      jQuery.ajax({
        type: 'POST',
        url,
        dataType: 'text',
        contentType: 'application/json; charset=utf-8',
        data: JSON.stringify({ code }),
        complete: function(xhr, status) {
          that.dissbuttonTarget.innerHTML = that.submitButtonOriginalTextValue;
          that.toggleSubmitButton(true);
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
