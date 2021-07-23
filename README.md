<p align="center">
  <br />
  <a title="Learn more about James" href="http://github.com/alefragnani/delphi-james"><img src="res/james-logo-readme.png" alt="James Logo" width="50%" /></a>
</p>

# What's new in James 3.0.2

* Delphi **Rio** Support
* Delphi **Sydney** Support
* Output file prefix setting

## Support

**James** is an utility created for **Delphi**. If you find it useful, please consider supporting it.

<table align="center" width="60%" border="0">
  <tr>
    <td>
      <a title="Paypal" href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=EP57F3B6FXKTU&lc=US&item_name=Alessandro%20Fragnani&item_number=delphi&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted"><img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif"/></a>
    </td>
    <td>
      <a title="Paypal" href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=EP57F3B6FXKTU&lc=BR&item_name=Alessandro%20Fragnani&item_number=delphi&currency_code=BRL&bn=PP%2dDonationsBF%3abtn_donate_SM%2egif%3aNonHosted"><img src="https://www.paypalobjects.com/pt_BR/i/btn/btn_donate_SM.gif"/></a>
    </td>
    <td>
      <a title="Patreon" href="https://www.patreon.com/alefragnani"><img src="res/become-a-patron-rounded.png"/></a>
    </td>
  </tr>
</table>

# James

It makes your life easier while switching from one project to another. 

If you see yourself _manually_ installing components and updating **Delphi** settings every time you have to _switch_ from one project to another, **James** may help you.

## Usage

**James** has two operation modes

### Loading Delphi Settings

All **Delphi** settings are stored in **Windows Registry**  (like the list of components that you have installed). **James** will load these settings and store in a local file:

```bat
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
If you want to change the file name by setting a prefix, just use the `-o` parameter after the `-l` parameter.

You add this `.james` file to the project's repository, and everyone which loads the repository will be able to _apply_ the settings.

### Applying Delphi Settings

Execute **James** with the `.james` file as parameter, to apply the settings stored:

```bat
    C:\Projects\FirstProject>james.exe -a:.james
```

The `-a:` parameter indicates that you want to _Apply_ the settings, in this case the `.james` file is located in the current folder.

> If you don't inform a `.james` file, it will try to load a file in the current folder.

## Stored Settings

**James** stores the following settings:

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
* Delphi Rio

## Installation

* Clone the repo `https://github.com/alefragnani/delphi-james.git`
* You will find **James** executable in the `bin` folder

### Registration

You may find usefull to register **James** in you `PATH` Environment Variable, so you can use it in any folder, with no need to type its full path. To register **James**, call it with `-r` parameter:

```bat
C:\delphi-james\bin\james.exe -r
```

# License

[GPLv3](LICENSE.md) &copy; Alessandro Fragnani

### 3rd Party resources

* **James** icon by [Webalys](https://www.iconfinder.com/icons/379338/dome_food_icon#size=128)
* `util\Converter.pas` and `util\Writer.pas` by [Embarcadero RTL.JSON Workbench Sample](http://docwiki.embarcadero.com/CodeExamples/Berlin/en/RTL.JSON_Workbench_Sample)
