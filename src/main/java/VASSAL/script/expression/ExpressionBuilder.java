/*
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
package VASSAL.script.expression;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BeanShellExpressionConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.counters.EditablePiece;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ButtonFactory;

/**
 * Interactively build inline(beanshell) expressions
 */
public class ExpressionBuilder extends JDialog {

  private static final long serialVersionUID = 1L;
  protected BeanShellExpressionConfigurer expression;
  protected String save;
  protected Configurer target;
  protected EditablePiece pieceTarget;

  public ExpressionBuilder(Configurer c, JDialog parent) {
    this(c, parent, null);
  }

  public ExpressionBuilder(Configurer c, JDialog parent, EditablePiece piece) {
    super(parent, "Expression Builder", true);
    target = c;
    pieceTarget = piece;
    save = target.getValueString();
    JPanel p = new JPanel(new MigLayout("wrap 1,fill"));

    String value = target.getValueString();

    if (value.startsWith("{") && value.endsWith("}")) {
      setExpression(value.substring(1, value.length()-1));
    }
    else {
      setExpression(convert(value));
    }

    p.add(expression.getControls(), "growx");

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
    add(p);

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
   * OK button pressed. Set the expression back into the target configurer
   * as an inline expression.
   */
  public void save() {
    final String expr = expression.getValueString().trim();
    if (expr.startsWith("{") && expr.endsWith("}")) {
      target.setValue(expr);
    }
    else {
      target.setValue("{" + expr + "}");
    }
    dispose();
  }

  public void cancel() {
    dispose();
  }

  /**
   * Convert an old-style $variable$ string to a BeanShell Expression
   * @param s Old-style string
   * @return expression
   */
  public String convert(String s) {
    return Expression.createExpression(s).toBeanShellString();
  }

  public void setExpression(String value) {
    if (expression == null) {
      String prompt = target.getName().length() == 0 ? "Expression:  " : target.getName();
      expression = new BeanShellExpressionConfigurer(null, prompt, value, pieceTarget);
    }
    expression.setValue(value);
  }

  public String getExpression() {
    return expression == null ? "" : expression.getValueString();
  }

}
