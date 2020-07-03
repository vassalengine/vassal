/*
 *
 * Copyright (c) 2008-2010 by Joel Uckelman
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

    MANAGE("manage"),
    LOAD("load"),
    EDIT("edit"),
    IMPORT("import"),
    NEW("new"),
    EDIT_EXT("edit-extension"),
    NEW_EXT("new-extension"),
    TRANSLATE("translate");

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

  public boolean standalone = false;

  public boolean builtInModule;
  public List<String> autoext;

  public int port = -1;

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
    this.standalone = lr.standalone;

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

    if (builtInModule) args.add("--auto");

    if (port >= 0) args.add("--port=" + port);

    if (autoext != null) {
      final StringBuilder sb = new StringBuilder("--auto-extensions=");

      final Iterator<String> i = autoext.iterator();
      sb.append(i.next());
      while (i.hasNext()) sb.append(',').append(i.next());
      args.add(sb.toString().replace(' ','_'));
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

  // FIXME: translate this somehow?
  private static final String help =
    "Usage:\n" +
      "  VASSAL -e [option]... module\n" +
      "  VASSAL -i [option]... module\n" +
      "  VASSAL -l [option]... module|save|log...\n" +
      "  VASSAL -n [option]...\n" +
      "  VASSAL -m\n" +
      "  VASSAL -h\n" +
      "  VASSAL --edit-extension [option]... module|extension...\n" +
      "  VASSAL --new-extension [option]...\n" +
      "\n" +
      "Options:\n" +
      "  -a, --auto          TODO\n" +
      "  -e, --edit          Edit a module\n" +
      "  -h, --help          Display this help and exit\n" +
      "  -i, --import        Import a non-VASSAL module\n" +
      "  -l, --load          Load a module and saved game or log\n" +
      "  -m, --manage        Use the module manager\n" +
      "  -n, --new           Create a new module\n" +
      "  -s, --standalone    Run in standalone mode\n" +
      "  --auto-extensions   TODO\n" +
      "  --edit-extension    Edit a module extension\n" +
      "  --new-extension     Create a new module extension\n" +
      "  --port              Set port for manager to listen on\n" +
      "  --version           Display version information and exit\n" +
      "  --                  Terminate the list of options\n" +
      "\n" +
      "VASSAL defaults to '-m' if no options are given.\n" +
      "\n";

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

    final LongOpt[] longOpts = new LongOpt[]{
      new LongOpt("auto",       LongOpt.NO_ARGUMENT, null, 'a'),
      new LongOpt("edit",       LongOpt.NO_ARGUMENT, null, 'e'),
      new LongOpt("help",       LongOpt.NO_ARGUMENT, null, 'h'),
      new LongOpt("import",     LongOpt.NO_ARGUMENT, null, 'i'),
      new LongOpt("load",       LongOpt.NO_ARGUMENT, null, 'l'),
      new LongOpt("manage",     LongOpt.NO_ARGUMENT, null, 'm'),
      new LongOpt("new",        LongOpt.NO_ARGUMENT, null, 'n'),
      new LongOpt("standalone", LongOpt.NO_ARGUMENT, null, 's'),
      new LongOpt("auto-extensions", LongOpt.REQUIRED_ARGUMENT, null, AUTO_EXT),
      new LongOpt("edit-extension", LongOpt.NO_ARGUMENT, null, EDIT_EXT),
      new LongOpt("new-extension", LongOpt.NO_ARGUMENT, null, NEW_EXT),
      new LongOpt("port", LongOpt.REQUIRED_ARGUMENT, null, PORT),
      new LongOpt("version", LongOpt.NO_ARGUMENT, null, VERSION),
      new LongOpt("translate", LongOpt.NO_ARGUMENT, null, TRANSLATE)
    };

    final Getopt g = new Getopt("VASSAL", args, ":aehilmn", longOpts);
    g.setOpterr(false);

    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {
      case AUTO_EXT:
        if (lr.autoext == null) lr.autoext = new ArrayList<>();
        for (String ext : g.getOptarg().split(",")) {
          lr.autoext.add(ext.replace("_"," "));
        }
        break;
      case EDIT_EXT:
        setMode(lr, Mode.EDIT_EXT);
        break;
      case NEW_EXT:
        setMode(lr, Mode.NEW_EXT);
        break;
      case PORT:
        try {
          lr.port = Integer.parseInt(g.getOptarg());
        }
        catch (NumberFormatException e) {
          die("LaunchRequest.bad_port", g.getOptarg());
        }

        if (lr.port < 49152 || lr.port > 65535) {
          die("LaunchRequest.bad_port", g.getOptarg());
        }
        break;
      case VERSION:
        System.err.println("VASSAL " + Info.getVersion());
        System.exit(0);
        break;
      case TRANSLATE:
        setMode(lr, Mode.TRANSLATE);
        break;
      case 'a':
        lr.builtInModule = true;
        break;
      case 'e':
        setMode(lr, Mode.EDIT);
        break;
      case 'h':
        System.err.print(help);
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
      case 's':
        lr.standalone = true;
        break;
      case ':':
        die("LaunchRequest.missing_argument", args[g.getOptind()-1]);
        break;
      case '?':
        // NB: getOptind() is not advanced if the unrecognized option
        // is short and bundled, so we must handle unrecognized long
        // options separately from unrecognized short options.
        die(
          "LaunchRequest.unrecognized_option",
          g.getOptopt() == 0 ?
            args[g.getOptind()-1] : '-' + String.valueOf((char) g.getOptopt())
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
            die("LaunchRequest.only_one", "module");
          lr.module = file;
        }
        else if (data instanceof ExtensionMetaData) {
          if (lr.extension != null) die("");
          lr.extension = file;
        }
        else if (data instanceof SaveMetaData) {
          if (lr.game != null)
            die("LaunchRequest.only_one", "saved game or log");
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
        else if (data instanceof ExtensionMetaData) {
        }
        else if (data instanceof SaveMetaData) {
        }
        else {
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
            die("LaunchRequest.only_one", "module");
          lr.module = file;
        }
        else if (data instanceof ExtensionMetaData) {
          if (lr.extension != null) die("");
          lr.extension = file;
        }
        else if (data instanceof SaveMetaData) {
        }
        else {
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
    }

    if (i < args.length) {
      die("LaunchRequest.excess_args", args[i]);
    }

    // other consistency checks
    if (lr.builtInModule) {
      if (lr.mode != Mode.LOAD) {
        die("LaunchRequest.only_in_mode", "--auto", Mode.LOAD.toString());
      }

      if (lr.module != null) {
        die("LaunchRequest.excess_args", args[i]);
      }
    }

    if (lr.autoext != null) {
      if (lr.mode != Mode.LOAD) {
        die("LaunchRequest.only_in_mode",
            "--auto-extensions", Mode.LOAD.toString());
      }

      if (lr.module != null) {
        die("LaunchRequest.excess_args", args[i]);
      }
    }

    return lr;
  }

  protected static void setMode(LaunchRequest lr, Mode mode)
                                                throws LaunchRequestException {
    if (lr.mode != null) die("LaunchRequest.only_one", "mode");
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
