/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Brent Easton
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
package VASSAL.script;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.StringReader;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.configure.Configurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidityChecker;
import VASSAL.tools.UniqueIdManager;

/**
 *
 *
 */
public class GeneralScript extends AbstractScript
   implements UniqueIdManager.Identifyable, ValidityChecker {

  private static UniqueIdManager idMgr = new UniqueIdManager("General-");

  public static String getConfigureTypeName() {
    return "General Script";
  }

  public GeneralScript() {
    super();
  }

  public String evaluate(PropertySource target) {
    return "";
  }

  public String getId() {
    return null;
  }

  public void setId(String id) {

  }

  protected String buildHeaderLine() {
    return "void " + getConfigureName() + "() {";
  }

  public String getFullScript() {
    return buildHeaderLine() + "\n" + getScript() + "\n}";
  }

  public CompileResult compile() {
    final String fullScript = getFullScript();
    return BeanShell.getInstance().compile(new StringReader(fullScript));
  }

  public Configurer getConfigurer() {
    return new ScriptConfigurer(this);
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Script.htm"); //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
    idMgr.remove(this);
  }

  public void addTo(Buildable parent) {
    idMgr.add(this);
  }

  public void validate(Buildable target, ValidationReport report) {
    idMgr.validate(this, report);
  }

  /**
   * Configure a Script
   */
  class ScriptConfigurer extends Configurer implements ActionListener {

    protected GeneralScript script;
    protected JPanel panel;
    protected JavaNameConfigurer nameConfig;
    protected StringConfigurer descConfig;
    protected TextConfigurer scriptConfig;
    protected JButton compileButton;
    protected JLabel error = new JLabel();
    protected JLabel headerLine = new JLabel();

    public ScriptConfigurer(GeneralScript s) {
      super(null, s.getConfigureName());
      script = s;
      setValue(script);
      script.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(final PropertyChangeEvent evt) {
          if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
            setName((String) evt.getNewValue());
          }
        }
      });

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      panel.setPreferredSize(new Dimension(800, 600));

      nameConfig = new JavaNameConfigurer(NAME, "Name:  ", script.getConfigureName());
      nameConfig.addPropertyChangeListener(new PropertyChangeListener(){
        public void propertyChange(PropertyChangeEvent e) {
          script.setAttribute(NAME, e.getNewValue());
          updateHeader();
        }});

      descConfig = new StringConfigurer(DESC, "Description:  ", script.getDescription());
      descConfig.addPropertyChangeListener(new PropertyChangeListener(){
        public void propertyChange(PropertyChangeEvent e) {
          script.setAttribute(DESC, e.getNewValue());
        }});

      headerLine.setText(buildHeaderLine());

      scriptConfig = new TextConfigurer(SCRIPT, "Script:  ", script.getScript());
      scriptConfig.addPropertyChangeListener(new PropertyChangeListener(){
        public void propertyChange(PropertyChangeEvent e) {
          script.setAttribute(SCRIPT, e.getNewValue());
        }});

      panel.add(nameConfig.getControls());
      panel.add(descConfig.getControls());
      panel.add(headerLine);
      panel.add(scriptConfig.getControls());

      Box compileBox = Box.createHorizontalBox();
      compileButton = new JButton("Compile");
      compileButton.addActionListener(this);
      compileBox.add(compileButton);
      compileBox.add(error);
      panel.add(compileBox);

    }

    public void updateHeader() {
      headerLine.setText(buildHeaderLine());
    }

    public GeneralScript getScript() {
      return script;
    }

    @Override
    public Component getControls() {
      return panel;
    }

    @Override
    public String getValueString() {
      return script.getConfigureName();
    }

    @Override
    public void setValue(String s) {
      throw new RuntimeException ("Can't set ScriptConfigurable from String");
    }

    /**
     * Compile the script and report errors
     */
    public void actionPerformed(ActionEvent e) {
      CompileResult r = script.compile();
      if (r.isSuccess()) {
        error.setText("");
      }
      else {
        error.setText(r.getMessage());
      }
    }
  }

}
