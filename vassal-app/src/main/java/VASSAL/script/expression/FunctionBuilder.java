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

import VASSAL.configure.Configurer;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;
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
  protected BeanShellExpressionConfigurer result;

  public FunctionBuilder(StringConfigurer c, JDialog parent, String function, String desc, String[] parmDesc, EditablePiece piece, String[] hints, BeanShellExpressionConfigurer.Option[] options, String selectedText) {
    super(parent, "Function Builder - "+function, true);
    target = c;
    targetPiece = piece;
    save = target.getValueString();
    this.function = function;
    setLayout(new MigLayout("fillx,ins 0"));

    JPanel p = new JPanel(new MigLayout("fillx", "[]rel[grow]"));

    p.add(new JLabel(desc), "span 2,align center,wrap,growx");
    for (int i=0; i < parmDesc.length; i++) {
      final BeanShellExpressionConfigurer config = new BeanShellExpressionConfigurer(null, "", "", targetPiece, options[i], this);
      if (i == 0 && isStringFunction() && selectedText != null) {
        config.setValue(selectedText);
      }
      configs.add(config);
      p.add(new JLabel(parmDesc[i] + ":"), "align right");
      p.add(config.getControls(), "align right,growx, wrap");
    }

    result = new BeanShellExpressionConfigurer(null, "", "", null, BeanShellExpressionConfigurer.Option.NONE, true);
    p.add(new JLabel("Result:"), "align right");
    p.add(result.getControls(), "align right,growx, wrap");

    if (hints != null && hints.length > 0) {
      final JPanel hintPanel = new JPanel(new MigLayout("ins 5"));
      hintPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
      for (String hint : hints) {
        hintPanel.add(new JLabel(hint), "wrap");
      }
      p.add(hintPanel, "span 2,growx,wrap");
    }

    JPanel buttonBox = new JPanel(new MigLayout("", "push[]rel[]rel[]push"));
    JButton okButton = ButtonFactory.getOkButton();
    okButton.addActionListener(e -> save());
    buttonBox.add(okButton);

    JButton cancelButton = ButtonFactory.getCancelButton();
    cancelButton.addActionListener(e -> cancel());
    buttonBox.add(cancelButton);

    JButton helpButton = ButtonFactory.getHelpButton();
    helpButton.addActionListener(e -> BrowserSupport.openURL(HelpFile.getReferenceManualPage("ExpressionBuilder.htm").getContents().toString()));
    buttonBox.add(helpButton);

    p.add(buttonBox, "span 2,align center,growx,wrap");
    add(p, "growx");

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
   * A child configurer has been updated, re-calculate the result, update the visualiser and re-validate it
   */
  public void update() {
    result.setValue(buildResult());
    result.validate();
  }

  /**
   * Ok button pressed. Set the expression back into the target configurer.
   * Note special handling for ternary "?" function. Only Ternary function, so no need to implement a general solution.
   */
  public void save() {
    target.setValue(buildResult());
    dispose();
  }

  protected String buildResult() {
    if (function.equals("?")) {
      return "((" + configs.get(0).getValueString() + ") ? " + getExpr(configs.get(1)) + " : " + getExpr(configs.get(2)) + ")";
    }
    else if (isStringFunction()) {
      return configs.get(0).getValueString() + getFunctionBody(true);
    }
    return getFunctionBody(false);
  }

  private String getFunctionBody(boolean skipFirstArgument) {
    StringBuilder result;
    result = new StringBuilder(function + "(");
    boolean first = true;
    for (int i=skipFirstArgument ? 1 : 0; i < configs.size(); i++) {
      BeanShellExpressionConfigurer fec = configs.get(i);
      if (!first) {
        result.append(",");
      }
      result.append(fec.getOption() == BeanShellExpressionConfigurer.Option.PME ? escape(fec.getValueString()): fec.getValueString());
      first = false;
    }
    result.append(")");
    return result.toString();
  }

  private String escape (String expr) {
    return "\"{" + expr.replace("\"", "\\\"") + "}\"";
  }

  private String getExpr (Configurer c) {
    final Expression e = Expression.createExpression("{"+c.getValueString()+"}");
    final boolean isAtomic = (e instanceof IntExpression) || (e instanceof StringExpression);
    return (isAtomic ? "" : "(") + c.getValueString() + (isAtomic ? "" : ")");
  }

  private boolean isStringFunction() {
    return function.startsWith(".");
  }

  public void cancel() {
    dispose();
  }

}