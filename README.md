# ![James](res/james64.png) James - The Delphi Project Manager _(beta)_

**James** is  intended to make your life easier while switching from one project to another. 

If you see yourself _manually_ installing components and updating Delphi settings every time you have to _switch_ from one project to another, **James** may help you.

> The project is in _early development_, so be patience :wink:

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
        "library_path": [
            "$(BDSLIB)\\$(Platform)\\release",
            "$(BDSUSERDIR)\\Imports",
            "$(BDS)\\Imports",
            "$(BDSCOMMONDIR)\\Dcp",
            "$(BDS)\\include",
            "C:\\Users\\alefr\\Documents\\Delphi\\SynEdit\\Lib"
        ],
        "environment_variables": [
            "$(PUBLIC)\\Documents\\Embarcadero\\InterBase\\redist\\InterBaseXE7\\IDE_spoof",
            "$(PATH)",
            "C:\\Users\\alefr\\Documents\\Delphi\\SynEdit\\Bin"
        ]
    }

```

You add this `.james` file to the project's repository, and everyone which loads the repository will be able to _apply_ the settings.

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
* Library Path (Win32)
* Environment Variables

## Compatibility

**James** currently supports the following Delphi versions:

* Delphi 2006 (Turbo Delphi)
* Delphi Berlin

> The _Edition_ is irrelevant

## Installation

* Clone the repo `https://github.com/alefragnani/delphi-james.git`
* You will find **James** executable in the `bin` folder

## 3rd Party resources

* **James** icon by [Webalys](https://www.iconfinder.com/icons/379338/dome_food_icon#size=128)
* `util\Converter.pas` and `util\Writer.pas` by [Embarcadero RTL.JSON Workbench Sample](http://docwiki.embarcadero.com/CodeExamples/Berlin/en/RTL.JSON_Workbench_Sample)

## TODO List

The project is in _early development_ and there is still more stuff to be added. Be patience :smile:

* Finish up **VisualJames** _(initially created just for testing purposes)_
* Add `verbose` mode 
* Support **64bits** settings when available
* Support other Delphi versions
* Support other relevant Delphi settings
* Support Relative Path

## License

[MIT](LICENSE.md) &copy; Alessandro Fragnani
