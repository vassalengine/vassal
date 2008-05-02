/*
 * $Id: Editor.java 3508 2008-05-01 15:11:42Z uckelman $
 *
 * Copyright (c) 2008 by Joel Uckelman 
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

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;

public class LaunchRequest implements Serializable {
  private static final long serialVersionUID = 1L;

  enum Mode {
    MANAGE,
    LOAD,
    EDIT,
    IMPORT,
    NEW,
    EDIT_EXT,
    NEW_EXT
  }

  public Mode mode;
  public File module;
  public File game;
  public File extension;

  public boolean standalone;

  public boolean auto;
  public List<String> autoext;
  public List<String> extract;

  public static LaunchRequest parseArgs(String[] args) {
    final LaunchRequest lr = new LaunchRequest();

    final LongOpt[] longOpts = new LongOpt[]{
      new LongOpt("auto",    LongOpt.NO_ARGUMENT,       null, 'a'),
      new LongOpt("edit",    LongOpt.NO_ARGUMENT,       null, 'e'),
      new LongOpt("extract", LongOpt.REQUIRED_ARGUMENT, null, 'x'), 
      new LongOpt("help",    LongOpt.NO_ARGUMENT,       null, 'h'),
      new LongOpt("import",  LongOpt.NO_ARGUMENT,       null, 'i'),
      new LongOpt("load",    LongOpt.NO_ARGUMENT,       null, 'l'),
      new LongOpt("new",     LongOpt.NO_ARGUMENT,       null, 'n'),
      new LongOpt("auto-extensions", LongOpt.REQUIRED_ARGUMENT, null, 2),
      new LongOpt("edit-extension", LongOpt.NO_ARGUMENT, null, 3),
      new LongOpt("new-extension", LongOpt.NO_ARGUMENT, null, 4),
      new LongOpt("standalone", LongOpt.NO_ARGUMENT, null, 5),
    };

    final Getopt g = new Getopt("VASSAL", args, ":aex:hiln");

    int c;
    String optarg;
    while ((c = g.getopt()) != -1) {
      switch (c) {
      case 2:   // auto-extensions
        if (lr.autoext == null) lr.autoext = new ArrayList<String>();
        for (String ext : g.getOptarg().split(",")) {
          lr.autoext.add(ext.replace("_"," "));
        }
        break;
      case 3:   // edit-extensions
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.EDIT_EXT; 
        break;
      case 4:   // new-extension
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.NEW_EXT;
        break;
      case 5:   // standalone
        lr.standalone = true;
        break;
      case 'a':
        lr.auto = true;
        break;
      case 'e':
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.EDIT;
        break;
      case 'x':
        if (lr.extract == null) lr.extract = new ArrayList<String>();
        lr.extract.add(g.getOptarg());
        break;
      case 'h':
        System.err.println("TODO: Command-line help.");
        System.exit(0);
        break;
      case 'i':
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.IMPORT;
        break;
      case 'l':
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.LOAD;
        break;
      case 'n':
        if (lr.mode != null)
          throw new IllegalArgumentException("only one mode");
        lr.mode = Mode.NEW;
        break;
      case ':':
      case '?':
        System.exit(1);
        break;
      default:
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
    case EDIT:
      if (i < args.length) {
        lr.module = new File(args[i++]);
        if (i < args.length) {
          lr.game = new File(args[i++]);
        }
      }
      else {
        throw new IllegalArgumentException("too few arguments");
      }
      break;
    case IMPORT:
    case NEW_EXT:
      if (i < args.length) {
        lr.module = new File(args[i++]);
      }
      else {
        throw new IllegalArgumentException("too few arguments");
      }
      break;
    case EDIT_EXT:
      if (i + 1 < args.length) {
        lr.module = new File(args[i++]);
        lr.extension = new File(args[i++]);
      }
      else {
        throw new IllegalArgumentException("too few arguments");
      }
      break;
    case NEW:
      break;
    }

    if (i < args.length) {
      throw new IllegalArgumentException("too many arguments");
    }   
 
    // other consistency checks
    if (lr.auto) {
      if (lr.mode != Mode.LOAD)
        throw new IllegalArgumentException("auto requires load mode"); 
      if (lr.module != null)
        throw new IllegalArgumentException("too many arguments");
    }  

    if (lr.autoext != null) {
      if (lr.mode != Mode.LOAD)
        throw new IllegalArgumentException("autoext requires load mode"); 
      if (lr.module != null)
        throw new IllegalArgumentException("too many arguments");
    }

    if (lr.standalone && lr.mode == Mode.MANAGE) {
      throw new IllegalArgumentException("standalone is not for manage mode");
    } 

    return lr;
  }
}
