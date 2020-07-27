/*
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
package VASSAL.build.module;

import java.awt.Component;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.HashMap;

import javax.swing.Box;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.GpIdSupport;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertySource;
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
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.UniqueIdManager;

public class PrototypeDefinition extends AbstractConfigurable
                                 implements UniqueIdManager.Identifyable,
                                            ValidityChecker {
  private String name = "Prototype"; //$NON-NLS-1$
  private java.util.Map<String,GamePiece> pieces =
    new HashMap<>();
  private String pieceDefinition;
  private static UniqueIdManager idMgr = new UniqueIdManager("prototype-"); //$NON-NLS-1$
  private PropertyChangeSupport propSupport = new PropertyChangeSupport(this);

  @Override
  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  @Override
  public String getConfigureName() {
    return name;
  }

  @Override
  public void setConfigureName(String s) {
    String oldName = name;
    this.name = s;
    propSupport.firePropertyChange(NAME_PROPERTY, oldName, name);
  }

  @Override
  public Configurer getConfigurer() {
    return new Config(this);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.htm", "Definition"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void remove(Buildable child) {
    idMgr.remove(this);
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public void add(Buildable child) {
  }

  @Override
  public String getId() {
    return null;
  }

  @Override
  public void setId(String id) {
  }

  @Override
  public void validate(Buildable target, ValidationReport report) {
    idMgr.validate(this, report);
  }

  @Override
  public void addTo(Buildable parent) {
    idMgr.add(this);
  }

  public GamePiece getPiece() {
    return getPiece(pieceDefinition);
  }

  /**
   * For the case when the piece definition is a Message Format, expand the definition using the given properties
   *
   * @param props
   * @return
   */
  public GamePiece getPiece(PropertySource props) {
    String def = props == null ? pieceDefinition : new FormattedString(pieceDefinition).getText(props);
    return getPiece(def);
  }

  protected GamePiece getPiece(String def) {
    GamePiece piece = pieces.get(def);
    if (piece == null && def != null) {
      try {
        final AddPiece comm = (AddPiece) GameModule.getGameModule().decode(def);
        if (comm == null) {
          ErrorDialog.dataError(new BadDataReport("Couldn't build piece ",def,null)); //$NON-NLS-1$
        }
        else {
          piece = comm.getTarget();
          piece.setState(comm.getState());
        }
      }
      catch (RuntimeException e) {
        ErrorDialog.dataError(new BadDataReport("Couldn't build piece",def,e));
      }
    }
    return piece;
  }

  public void setPiece(GamePiece p) {
    pieceDefinition = p == null ? null : GameModule.getGameModule().encode(new AddPiece(p));
    pieces.clear();
  }

  @Override
  public void build(Element e) {
    if (e != null) {
      setConfigureName(e.getAttribute(NAME_PROPERTY));
      pieceDefinition = Builder.getText(e);
    }
  }

  @Override
  public Element getBuildElement(Document doc) {
    Element el = doc.createElement(getClass().getName());
    el.setAttribute(NAME_PROPERTY, name);
    el.appendChild(doc.createTextNode(pieceDefinition));
    return el;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Prototype.component_type"); //$NON-NLS-1$
  }

  public static class Config extends Configurer {
    private Box box;
    private PieceDefiner pieceDefiner;
    private StringConfigurer name;
    private PrototypeDefinition def;

    public Config(PrototypeDefinition def) {
      super(null, null, def);
      box = Box.createVerticalBox();
      name = new StringConfigurer(null, Resources.getString(Resources.NAME_LABEL), def.name);
      box.add(name.getControls());
      pieceDefiner = new Definer(GameModule.getGameModule().getGpIdSupport());
      pieceDefiner.setPiece(def.getPiece());
      box.add(pieceDefiner);
      this.def = def;
    }

    @Override
    public Object getValue() {
      if (def != null) {
        def.setPiece(pieceDefiner.getPiece());
        def.setConfigureName(name.getValueString());
      }
      return def;
    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getValueString() {
      return null;
    }

    @Override
    public void setValue(String s) {
    }
    public static class Definer extends PieceDefiner {
      private static final long serialVersionUID = 1L;

      public Definer(GpIdSupport s) {
        super(s);
      }

      @Override
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

      @Override
      protected void removeTrait(int index) {
        if (index > 0) {
          super.removeTrait(index);
        }
      }

      private static class Plain extends BasicPiece {
        public Plain() {
          super(ID + ";;;;"); //$NON-NLS-1$
        }

        @Override
        public String getDescription() {
          return ""; //$NON-NLS-1$
        }

        @Override
        public PieceEditor getEditor() {
          return null;
        }
      }
    }
  }

  /*
   * Implement Translatable - Since PrototypeDefinition implements its
   * own configurer, methods below here will only ever be called by the
   * translation subsystem.
   */

  @Override
  public void setAttribute(String attr, Object value) {
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[0];
  }

  /*
   * Redirect getAttributeValueString() to return the attribute
   * values for the enclosed pieces
   */
  @Override
  public String getAttributeValueString(String attr) {
    return getI18nData().getLocalUntranslatedValue(attr);
  }

  @Override
  public ComponentI18nData getI18nData() {
    /*
     * Prototype definition may change due to editing, so no caching
     */
    return new ComponentI18nData(this, getPiece());
  }
}
