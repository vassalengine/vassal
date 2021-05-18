/*
 * Copyright (c) 2008-2021 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.launch;

import VASSAL.build.GameModule;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import VASSAL.Info;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.ExtensionMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.ModuleMetaData;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.i18n.Resources;

/**
 * Encapsulates and parses command-line arguments.
 * <code>args</code> and <code>LaunchRequest.parseArgs(args).toArgs()</code>
 * are equivalent (though perhaps not equal) argument lists.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class LaunchRequest implements Serializable {
  private static final long serialVersionUID = 1L;

  enum Mode {
    MANAGE("manage"), //NON-NLS
    LOAD("load"), //NON-NLS
    EDIT("edit"), //NON-NLS
    IMPORT("import"), //NON-NLS
    NEW("new"), //NON-NLS
    EDIT_EXT("edit-extension"), //NON-NLS
    NEW_EXT("new-extension"), //NON-NLS
    TRANSLATE("translate"), //NON-NLS
    UPDATE_MOD("update-module"), //NON-NLS
    UPDATE_EXT("update-extension"), //NON-NLS
    UPDATE_GAME("update-game"); //NON-NLS

    private final String prettyName;

    Mode(String prettyName) {
      this.prettyName = prettyName;
    }

    @Override
    public String toString() {
      return prettyName;
    }
  }

  public Mode mode;
  public File module;
  public File game;
  public File extension;
  public File importFile;

  public boolean builtInModule;
  public List<String> autoext;

  public long key;

  public LaunchRequest() {
    this(null, null);
  }

  public LaunchRequest(Mode mode) {
    this(mode, null, null);
  }

  public LaunchRequest(Mode mode, File module) {
    this(mode, module, null);
  }

  public LaunchRequest(Mode mode, File module, File other) {
    this.mode = mode;
    this.module = module;

    if (mode == Mode.EDIT_EXT) extension = other;
    else game = other;
  }

  public LaunchRequest(LaunchRequest lr) {
    this.mode = lr.mode;
    this.module = lr.module;
    this.game = lr.game;
    this.extension = lr.extension;
    this.importFile = lr.importFile;

    this.builtInModule = lr.builtInModule;

    if (lr.autoext != null) this.autoext = new ArrayList<>(lr.autoext);
  }

  /**
   * Create an argument array equivalent to this <code>LaunchRequest</code>.
   *
   * @return an array which would be parsed to this <code>LaunchRequest</code>
   */
  public String[] toArgs() {
    final ArrayList<String> args = new ArrayList<>();

    args.add("--" + mode);

    if (builtInModule) args.add("--auto"); //NON-NLS

    if (autoext != null) {
      final StringBuilder sb = new StringBuilder("--auto-extensions="); //NON-NLS

      final Iterator<String> i = autoext.iterator();
      sb.append(i.next());
      while (i.hasNext()) sb.append(',').append(i.next());
      args.add(sb.toString().replace(' ', '_'));
    }

    args.add("--");

    if (module != null) {
      args.add(module.getPath());
      if (game != null) {
        args.add(game.getPath());
      }
      else if (extension != null) {
        args.add(extension.getPath());
      }
    }
    else if (importFile != null) {
      args.add(importFile.getPath());
    }

    return args.toArray(new String[0]);
  }

  /**
   * @return Usage string
   */
  private static String helpMeSpock() {
    /*
      NB: Resources has a configurer, which means that calling its methods
      initializes the Swing graphics subsystem. The init method of StartUp
      must be called before that if we're going to have a Swing GUI, as it
      sets some system properties we require. Thus, the help string cannot
      be a static member of LaunchRequest if any of it is translated, as
      that would trigger the initialization of Swing before we've called
      StartUp.initSystemProperties(). You have been warned. :)
    */
    return Resources.getString("LaunchRequest.usage") + ":\n" +
      "  VASSAL -c\n" + //NON-NLS
      "  VASSAL -e [option]... module\n" + //NON-NLS
      "  VASSAL -i [option]... module\n" + //NON-NLS
      "  VASSAL -l [option]... module|save|log...\n" + //NON-NLS
      "  VASSAL -n [option]...\n" + //NON-NLS
      "  VASSAL -m\n" + //NON-NLS
      "  VASSAL -h\n" + //NON-NLS
      "  VASSAL --edit-extension [option]... module|extension...\n" + //NON-NLS
      "  VASSAL --new-extension [option]...\n" + //NON-NLS
      "\n" +
      Resources.getString("LaunchRequest.options") + ":\n" +
      "  -a, --auto          TODO\n" + //NON-NLS
      "  -c, --chatlog       " + Resources.getString("LaunchRequest.chatlog") + "\n" + //NON-NLS
      "  -e, --edit          " + Resources.getString("LaunchRequest.edit") + "\n" + //NON-NLS
      "  -h, --help          " + Resources.getString("LaunchRequest.help") + "\n" + //NON-NLS
      "  -i, --import        " + Resources.getString("LaunchRequest.import") + "\n" + //NON-NLS
      "  -l, --load          " + Resources.getString("LaunchRequest.load") + "\n" + //NON-NLS
      "  -m, --manage        " + Resources.getString("LaunchRequest.manage") + "\n" + //NON-NLS
      "  -n, --new           " + Resources.getString("LaunchRequest.new") + "\n" + //NON-NLS
      "  --auto-extensions   TODO\n" + //NON-NLS
      "  --edit-extension    " + Resources.getString("LaunchRequest.extension") + "\n" + //NON-NLS
      "  --new-extension     " + Resources.getString("LaunchRequest.new_extension") + "\n" + //NON-NLS
      "  --version           " + Resources.getString("LaunchRequest.version") + "\n" + //NON-NLS
      "  --                  " + Resources.getString("LaunchRequest.terminate") + "\n" + //NON-NLS
      "\n" +
      Resources.getString("LaunchRequest.default") + "\n" +
      "\n";
  }

  /**
   * Parse an argument array to a <code>LaunchRequest</code>.
   *
   * @param args an array of command-line arguments
   * @return a <code>LaunchRequest</code> equivalent to <code>args</code>
   * @throws LaunchRequestException when parsing fails
   */
  public static LaunchRequest parseArgs(String[] args)
                                                throws LaunchRequestException {
    final LaunchRequest lr = new LaunchRequest();

    final int AUTO_EXT = 2;
    final int EDIT_EXT = 3;
    final int NEW_EXT = 4;
    final int PORT = 5;
    final int VERSION = 6;
    final int TRANSLATE = 7;
    final int UPDATE_MOD = 8;
    final int UPDATE_EXT = 9;
    final int UPDATE_GAME = 10;
    final int STANDALONE = 11;

    final LongOpt[] longOpts = {
      new LongOpt("auto",       LongOpt.NO_ARGUMENT, null, 'a'), //NON-NLS
      new LongOpt("chatlog",    LongOpt.NO_ARGUMENT, null, 'c'), //NON-NLS
      new LongOpt("edit",       LongOpt.NO_ARGUMENT, null, 'e'), //NON-NLS
      new LongOpt("help",       LongOpt.NO_ARGUMENT, null, 'h'), //NON-NLS
      new LongOpt("import",     LongOpt.NO_ARGUMENT, null, 'i'), //NON-NLS
      new LongOpt("load",       LongOpt.NO_ARGUMENT, null, 'l'), //NON-NLS
      new LongOpt("manage",     LongOpt.NO_ARGUMENT, null, 'm'), //NON-NLS
      new LongOpt("new",        LongOpt.NO_ARGUMENT, null, 'n'), //NON-NLS
      new LongOpt("auto-extensions", LongOpt.REQUIRED_ARGUMENT, null, AUTO_EXT), //NON-NLS
      new LongOpt("edit-extension", LongOpt.NO_ARGUMENT, null, EDIT_EXT), //NON-NLS
      new LongOpt("new-extension", LongOpt.NO_ARGUMENT, null, NEW_EXT), //NON-NLS
      new LongOpt("port", LongOpt.REQUIRED_ARGUMENT, null, PORT), //NON-NLS
      new LongOpt("version", LongOpt.NO_ARGUMENT, null, VERSION), //NON-NLS
      new LongOpt("translate", LongOpt.NO_ARGUMENT, null, TRANSLATE), //NON-NLS
      new LongOpt("update-module", LongOpt.NO_ARGUMENT, null, UPDATE_MOD), //NON-NLS
      new LongOpt("update-extension", LongOpt.NO_ARGUMENT, null, UPDATE_EXT), //NON-NLS
      new LongOpt("update-game", LongOpt.NO_ARGUMENT, null, UPDATE_GAME), //NON-NLS
      new LongOpt("standalone", LongOpt.NO_ARGUMENT, null, STANDALONE) //NON-NLS
    };

    final Getopt g = new Getopt("VASSAL", args, ":aehilmn", longOpts); //NON-NLS
    g.setOpterr(false);

    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {
      case AUTO_EXT:
        if (lr.autoext == null) lr.autoext = new ArrayList<>();
        for (final String ext : g.getOptarg().split(",")) {
          lr.autoext.add(ext.replace("_", " "));
        }
        break;
      case EDIT_EXT:
        setMode(lr, Mode.EDIT_EXT);
        break;
      case NEW_EXT:
        setMode(lr, Mode.NEW_EXT);
        break;
      case PORT:
        // does nothing
        break;
      case VERSION:
        System.err.println("VASSAL " + Info.getVersion()); //NON-NLS
        System.exit(0);
        break;
      case TRANSLATE:
        setMode(lr, Mode.TRANSLATE);
        break;
      case UPDATE_MOD:
        setMode(lr, Mode.UPDATE_MOD);
        break;
      case UPDATE_EXT:
        setMode(lr, Mode.UPDATE_EXT);
        break;
      case UPDATE_GAME:
        setMode(lr, Mode.UPDATE_GAME);
        break;
      case STANDALONE:
        // does nothing
        break;
      case 'a':
        lr.builtInModule = true;
        break;
      case 'c':
        GameModule.setErrorLogToChat(true);
        break;
      case 'e':
        setMode(lr, Mode.EDIT);
        break;
      case 'h':
        System.err.print(helpMeSpock());
        System.exit(0);
        break;
      case 'i':
        setMode(lr, Mode.IMPORT);
        break;
      case 'l':
        setMode(lr, Mode.LOAD);
        break;
      case 'm':
        setMode(lr, Mode.MANAGE);
        break;
      case 'n':
        setMode(lr, Mode.NEW);
        break;
      case ':':
        die("LaunchRequest.missing_argument", args[g.getOptind() - 1]);
        break;
      case '?':
        // NB: getOptind() is not advanced if the unrecognized option
        // is short and bundled, so we must handle unrecognized long
        // options separately from unrecognized short options.
        die(
          "LaunchRequest.unrecognized_option",
          g.getOptopt() == 0 ?
            args[g.getOptind() - 1] : '-' + String.valueOf((char) g.getOptopt())
        );
        break;
      default:
        // should never happen
        throw new IllegalStateException();
      }
    }

    int i = g.getOptind();

    // load by default if a non-option argument is given; otherwise, manage
    if (lr.mode == null) {
      lr.mode = i < args.length ? Mode.LOAD : Mode.MANAGE;
    }

    // get the module and game, if specified
    switch (lr.mode) {
    case MANAGE:
      break;
    case LOAD:
      while (i < args.length) {
        final File file = new File(args[i++]);
        final AbstractMetaData data = MetaDataFactory.buildMetaData(file);
        if (data instanceof ModuleMetaData) {
          if (lr.module != null)
            die("LaunchRequest.only_one", "module"); //NON-NLS
          lr.module = file;
        }
        else if (data instanceof ExtensionMetaData) {
          if (lr.extension != null) die("");
          lr.extension = file;
        }
        else if (data instanceof SaveMetaData) {
          if (lr.game != null)
            die("LaunchRequest.only_one", "saved game or log"); //NON-NLS
          lr.game = file;
        }
        else {
          die("LaunchRequest.unknown_file_type", file.toString());
        }
      }

      if (!lr.builtInModule && lr.module == null && lr.game == null) {
        die("LaunchRequest.missing_module");
      }
      break;
    case IMPORT:
      if (i < args.length) {
        lr.importFile = new File(args[i++]);
      }
      else {
        die("LaunchRequest.missing_module");
      }
      break;
    case EDIT:
    case NEW_EXT:
      if (i < args.length) {
        final File file = new File(args[i++]);
        final AbstractMetaData data = MetaDataFactory.buildMetaData(file);
        if (data instanceof ModuleMetaData) {
          lr.module = file;
        }
        else if (!(data instanceof ExtensionMetaData) && !(data instanceof SaveMetaData)) {
          die("LaunchRequest.unknown_file_type", file.toString());
        }
      }
      else {
        die("LaunchRequest.missing_module");
      }
      break;
    case EDIT_EXT:
      while (i < args.length) {
        final File file = new File(args[i++]);
        final AbstractMetaData data = MetaDataFactory.buildMetaData(file);
        if (data instanceof ModuleMetaData) {
          if (lr.module != null)
            die("LaunchRequest.only_one", "module"); //NON-NLS
          lr.module = file;
        }
        else if (data instanceof ExtensionMetaData) {
          if (lr.extension != null) die("");
          lr.extension = file;
        }
        else if (!(data instanceof SaveMetaData)) {
          die("LaunchRequest.unknown_file_type", file.toString());
        }
      }

      if (lr.module == null) {
        die("LaunchRequest.missing_module");
      }

      if (lr.extension == null) {
        die("LaunchRequest.missing_extension");
      }
      break;
    case NEW:
    case TRANSLATE:
      break;
    case UPDATE_MOD:
      if (i < args.length) {
        lr.module = new File(args[i++]);
      }
      else {
        die("LaunchRequest.missing_module");
      }
      break;
    case UPDATE_EXT:
      if (i < args.length) {
        lr.extension = new File(args[i++]);
      }
      else {
        die("LaunchRequest.missing_module");
      }
      break;
    case UPDATE_GAME:
      if (i < args.length) {
        lr.game = new File(args[i++]);
      }
      else {
        die("LaunchRequest.missing_module");
      }
      break;
    }

    if (i < args.length) {
      die("LaunchRequest.excess_args", args[i]);
    }

    // other consistency checks
    if (lr.builtInModule) {
      if (lr.mode != Mode.LOAD) {
        die("LaunchRequest.only_in_mode", "--auto", Mode.LOAD.toString()); //NON-NLS
      }

      if (lr.module != null) {
        die("LaunchRequest.excess_args", args[i]);
      }
    }

    if (lr.autoext != null) {
      if (lr.mode != Mode.LOAD) {
        die("LaunchRequest.only_in_mode",
            "--auto-extensions", Mode.LOAD.toString()); //NON-NLS
      }

      if (lr.module != null) {
        die("LaunchRequest.excess_args", args[i]);
      }
    }

    return lr;
  }

  protected static void setMode(LaunchRequest lr, Mode mode)
                                                throws LaunchRequestException {
    if (lr.mode != null) die("LaunchRequest.only_one", "mode"); //NON-NLS
    lr.mode = mode;
  }

  /**
   * Throws a {@link LaunchRequestException}.
   *
   * @param key {@link Resources} key
   * @param vals {@link Resources} arguments
   * @throws LaunchRequestException always
   */
  protected static void die(String key, String... vals)
                                                throws LaunchRequestException {
    throw new LaunchRequestException(key, vals);
  }
}
