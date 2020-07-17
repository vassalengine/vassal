/*
 *
 * Copyright (c) 2008-2009 Brent Easton
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
/*
 * FormattedStringConfigurer.
 * Extended version of StringConfigure that provides a drop down list of options that can
 * be inserted into the string
 */
package VASSAL.script.expression;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BeanShellExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.counters.EditablePiece;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ButtonFactory;

public class FunctionBuilder extends JDialog {

  private static final long serialVersionUID = 1L;
  protected String save;
  protected StringConfigurer target;
  protected String function;
  protected List<BeanShellExpressionConfigurer> configs = new ArrayList<>();
  protected EditablePiece targetPiece;

  public FunctionBuilder(StringConfigurer c, JDialog parent, String function, String desc, String[] parmDesc, EditablePiece piece) {
    super(parent, "Function Builder - "+function, true);
    target = c;
    targetPiece = piece;
    save = target.getValueString();
    this.function = function;
    setLayout(new MigLayout("fillx,ins 0"));

    JPanel p = new JPanel(new MigLayout("wrap 1,fillx"));

    p.add(new JLabel(desc), "align center");
    for (String s : parmDesc) {
      final BeanShellExpressionConfigurer config = new BeanShellExpressionConfigurer(null, s + ":  ", "", targetPiece);
      configs.add(config);
      p.add(config.getControls(), "align right,growx");
    }

    JPanel buttonBox = new JPanel(new MigLayout("", "[]rel[]rel[]"));
    JButton okButton = ButtonFactory.getOkButton();
    okButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        save();
      }
    });
    buttonBox.add(okButton);

    JButton cancelButton = ButtonFactory.getCancelButton();
    cancelButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        cancel();
      }
    });
    buttonBox.add(cancelButton);

    JButton helpButton = ButtonFactory.getHelpButton();
    helpButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        BrowserSupport.openURL(HelpFile.getReferenceManualPage("ExpressionBuilder.htm").getContents().toString());
      }
    });
    buttonBox.add(helpButton);

    p.add(buttonBox, "align center");
    add(p,"growx");

    pack();
    setLocationRelativeTo(getParent());
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent we) {
        cancel();
      }
    });
  }

  /**
   * Ok button pressed. Set the expression back into the target configurer.
   */
  public void save() {
    String result = function + "(";
    boolean first = true;
    for (BeanShellExpressionConfigurer fec : configs) {
      if (! first) {
        result += ",";
      }
      result += fec.getValueString();
      first = false;
    }
    result += ")";
    target.setValue(result);
    dispose();
  }

  public void cancel() {
    dispose();
  }

}
