package VASSAL.build.module;

import java.awt.Component;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.Box;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.AddPiece;
import VASSAL.configure.Configurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidityChecker;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceDefiner;
import VASSAL.counters.PieceEditor;
import VASSAL.counters.Properties;
import VASSAL.tools.UniqueIdManager;

/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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


public class PrototypeDefinition implements Configurable, UniqueIdManager.Identifyable, ValidityChecker {
  private String name = "Prototype";
  private GamePiece piece;
  private String pieceDefinition;
  private static UniqueIdManager idMgr = new UniqueIdManager("prototype-");

  private PropertyChangeSupport propSupport = new PropertyChangeSupport(this);

  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public String getConfigureName() {
    return name;
  }

  public void setConfigureName(String s) {
    String oldName = name;
    this.name = s;
    propSupport.firePropertyChange(NAME_PROPERTY, oldName, name);
  }

  public Configurer getConfigurer() {
    return new Config(this);
  }

  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GameModule.htm"), "#Definition");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public void remove(Buildable child) {
    idMgr.remove(this);
  }

  public void removeFrom(Buildable parent) {
  }

  public void add(Buildable child) {
  }

  public String getId() {
    return null;
  }

  public void setId(String id) {
  }

  public void validate(Buildable target, ValidationReport report) {
    idMgr.validate(this,report);
  }

  public void addTo(Buildable parent) {
    idMgr.add(this);
  }

  public GamePiece getPiece() {
    if (piece == null
        && pieceDefinition != null) {
      AddPiece comm = (AddPiece) GameModule.getGameModule().decode(pieceDefinition);
      if (comm == null) {
        System.err.println("Couldn't build piece " + pieceDefinition);
        pieceDefinition = null;
      }
      else {
        piece = comm.getTarget();
        piece.setState(comm.getState());
      }
    }
    return piece;
  }

  public void setPiece(GamePiece p) {
    pieceDefinition = p == null ? null
        : GameModule.getGameModule().encode(new AddPiece(p));
    piece = null;
  }

  public void build(Element e) {
    if (e != null) {
      setConfigureName(e.getAttribute(NAME_PROPERTY));
      pieceDefinition = Builder.getText(e);
    }
  }

  public Element getBuildElement(Document doc) {
    Element el = doc.createElement(getClass().getName());
    el.setAttribute(NAME_PROPERTY, name);
    el.appendChild(doc.createTextNode(piece == null ? pieceDefinition : GameModule.getGameModule().encode(new AddPiece(piece))));
    return el;
  }

  public static String getConfigureTypeName() {
    return "Definition";
  }

  public static class Config extends Configurer {
    private Box box;
    private PieceDefiner pieceDefiner;
    private StringConfigurer name;
    private PrototypeDefinition def;

    public Config(PrototypeDefinition def) {
      super(null, null, def);
      box = Box.createVerticalBox();
      name = new StringConfigurer(null, "Name", def.name);
      box.add(name.getControls());
      pieceDefiner = new Definer();
      pieceDefiner.setPiece(def.getPiece());
      box.add(pieceDefiner);
      this.def = def;
    }

    public Object getValue() {
      if (def != null) {
        def.setPiece(pieceDefiner.getPiece());
        def.setConfigureName(name.getValueString());
      }
      return def;
    }

    public Component getControls() {
      return box;
    }

    public String getValueString() {
      return null;
    }

    public void setValue(String s) {
    }

    public static class Definer extends PieceDefiner {
      public void setPiece(GamePiece piece) {
        if (piece != null) {
          GamePiece inner = Decorator.getInnermost(piece);
          if (!(inner instanceof Plain)) {
            Plain plain = new Plain();
            Object outer = inner.getProperty(Properties.OUTER);
            if (outer instanceof Decorator) {
              ((Decorator) outer).setInner(plain);
            }
            piece = Decorator.getOutermost(plain);
          }
        }
        else {
          piece = new Plain();
        }
        super.setPiece(piece);
      }

      protected void removeTrait(int index) {
        if (index > 0) {
          super.removeTrait(index);
        }
      }

      private static class Plain extends BasicPiece {
        public Plain() {
          super(ID + ";;;;");
        }

        public String getDescription() {
          return "";
        }

        public PieceEditor getEditor() {
          return null;
        }
      }
    }
  }
}
