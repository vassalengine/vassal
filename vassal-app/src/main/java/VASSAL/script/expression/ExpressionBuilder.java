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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.BeanShellExpressionConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.counters.EditablePiece;
import VASSAL.i18n.Resources;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ButtonFactory;
import net.miginfocom.swing.MigLayout;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

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
    super(parent, Resources.getString("Editor.ExpressionBuilder.component_type"), true);
    target = c;
    pieceTarget = piece;
    save = target.getValueString();
    final JPanel p = new JPanel(new MigLayout("wrap 1,fill")); //NON-NLS

    final String value = target.getValueString();

    if (value.startsWith("{") && value.endsWith("}")) {
      setExpression(value.substring(1, value.length() - 1));
    }
    else {
      setExpression(convert(value));
    }

    p.add(expression.getControls(), "growx"); //NON-NLS

    final JPanel buttonBox = new JPanel(new MigLayout("", "[]rel[]rel[]")); //NON-NLS
    final JButton okButton = ButtonFactory.getOkButton();
    okButton.addActionListener(e -> save());
    buttonBox.add(okButton);

    final JButton cancelButton = ButtonFactory.getCancelButton();
    cancelButton.addActionListener(e -> cancel());
    buttonBox.add(cancelButton);

    final JButton helpButton = ButtonFactory.getHelpButton();
    helpButton.addActionListener(e -> BrowserSupport.openURL(HelpFile.getReferenceManualPage("ExpressionBuilder.html").getContents().toString())); //NON-NLS
    buttonBox.add(helpButton);

    p.add(buttonBox, "align center"); //NON-NLS
    add(p);

    // Default actions for Enter/Cancel
    getRootPane().setDefaultButton(okButton);
    getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
      KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "Cancel"); //$NON-NLS-1$
    getRootPane().getActionMap().put("Cancel", new AbstractAction() {
      @Override
      public void actionPerformed(ActionEvent e) {
        cancelButton.doClick();
      }
    });

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
      final String prompt = target.getName().length() == 0 ? Resources.getString("Editor.ExpressionBuilder.expression") : target.getName();
      expression = new BeanShellExpressionConfigurer(null, prompt, value, pieceTarget);
    }
    expression.setValue(value);
  }

  public String getExpression() {
    return expression == null ? "" : expression.getValueString();
  }

}
