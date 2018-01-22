# ![James](res/james64.png) James - The Delphi Project Manager

**James** is  intended to make your life easier while switching from one project to another. 

If you see yourself _manually_ installing components and updating Delphi settings every time you have to _switch_ from one project to another, **James** may help you.

## Usage

**James** has two operation modes

### Loading Delphi Settings

All **Delphi** settings are stored in **Windows Registry**  (like the list of components that you have installed). **James** will load these settings and store in a local file:

```
    C:\Projects\FirstProject>james.exe -l:Berlin
```

The `-l:` parameter indicates that you want to _Load_ the settings, and in this case, you asked for _Berlin_ version.

It will save a file called `.james` in the same folder as you execute. It is a `JSON` file containing all Delphi settings that were loaded.  Here is an example:

```json
    {
        "version": "Delphi Berlin",
        "known_packages": [
            "$(BDS)\\Bin\\dclMetropolisUILiveTile240.bpl",
            "$(BDSBIN)\\dclxml240.bpl",
            "...",
            "C:\\Users\\alefr\\Documents\\Delphi\\SynEdit\\Bin\\SynEdit_D101B.bpl"
        ],
        "library_path": {
          "win32": [
            "$(BDSLIB)\\$(Platform)\\release",
            "$(BDSUSERDIR)\\Imports",
            "$(BDS)\\Imports",
            "$(BDSCOMMONDIR)\\Dcp",
            "$(BDS)\\include",
            "C:\\Users\\alefr\\Documents\\Delphi\\SynEdit\\Lib"
          ]
        },
        "environment_variables": [
            "$(PUBLIC)\\Documents\\Embarcadero\\InterBase\\redist\\InterBaseXE7\\IDE_spoof",
            "$(PATH)",
            "C:\\Users\\alefr\\Documents\\Delphi\\SynEdit\\Bin"
        ]
    }

```

You add this `.james` file to the project's repository, and everyone which loads the repository will be able to _apply_ the settings.

> _Breaking in v0.3.0:_ The file format has been updated to support multiplatform in `library_path`. So, if you have any `.james` file for Delphi Berlin, you should update it as the example above _(note the `win32` node)_.

### Applying Delphi Settings

Execute **James** with the `.james` file as parameter, to apply the settings stored:

```
    C:\Projects\FirstProject>james.exe -a:.james
```

The `-a:` parameter indicates that you want to _Apply_ the settings, in this case the `.james` file is located in the current folder.

> If you don't pass a `.james` file, it will try to load a file in the current folder.

## Stored Settings

For now **James** stores the following settings _(new comming soon)_:

* Installed Packages
* Library Path
* Environment Variables

## Compatibility

**James** currently supports the following Delphi versions:

* Delphi 5
* Delphi 2006 (Turbo Delphi)
* Delphi Seattle
* Delphi Berlin
* Delphi Tokyo

> The _Edition_ is irrelevant

## Installation

* Clone the repo `https://github.com/alefragnani/delphi-james.git`
* You will find **James** executable in the `bin` folder

## 3rd Party resources

* **James** icon by [Webalys](https://www.iconfinder.com/icons/379338/dome_food_icon#size=128)
* `util\Converter.pas` and `util\Writer.pas` by [Embarcadero RTL.JSON Workbench Sample](http://docwiki.embarcadero.com/CodeExamples/Berlin/en/RTL.JSON_Workbench_Sample)

## License

[MIT](LICENSE.md) &copy; Alessandro Fragnani

---

[![Paypal Donations](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=EP57F3B6FXKTU&lc=US&item_name=Alessandro%20Fragnani&item_number=delphi&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted) a :coffee: and you will help me to keep working on this project :wink:

[![Paypal Donations](https://www.paypalobjects.com/pt_BR/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=EP57F3B6FXKTU&lc=BR&item_name=Alessandro%20Fragnani&item_number=delphi&currency_code=BRL&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted) uma :coffee: e você vai me ajudar a continuar trabalhando neste projeto :wink: