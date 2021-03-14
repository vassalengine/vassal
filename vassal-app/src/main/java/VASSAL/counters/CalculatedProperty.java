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

package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.List;

import java.util.Objects;
import javax.swing.KeyStroke;

import VASSAL.build.BadDataReport;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.BeanShellExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.BeanShellExpression;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.SequenceEncoder;

/**
 * Conditional Marker
 * A marker with a variable value depending on conditions.
 * */
public class CalculatedProperty extends Decorator implements EditablePiece, Loopable {

  public static final String ID = "calcProp;"; // NON-NLS

  protected static int counter = 0;

  protected String name = "";
  protected Expression expression;
  protected String description = "";

  public CalculatedProperty() {
    this(ID, null);
  }

  public CalculatedProperty(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(name)
      .append(getExpression())
      .append(description);
    return ID + se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.CalculatedProperty.trait_description", name, description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("CalculatedProperty.html"); // NON-NLS
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    name = st.nextToken("");
    expression = BeanShellExpression.createExpression(st.nextToken(""), true);
    description = st.nextToken("");
  }

  protected String getExpression() {
    return expression.getExpression();
  }


  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return List.of(getExpression());
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return List.of(name);
  }


  /**
   * Return the value of this trait's property.
   * Evaluating Expressions can lead to infinite loops and
   * eventually a Stack Overflow. Trap and report this
   */
  @Override
  public Object getProperty(Object key) {
    Object result = "";
    if (name.length() > 0 && name.equals(key)) {
      // Do not attempt to evaluate a Calculated Property if this piece is not on a map
      if (getMap() == null) {
        return "";
      }
      else {
        try {
          RecursionLimiter.startExecution(this);
          result = evaluate();
          return result;
        }
        catch (RecursionLimitException e) {
          RecursionLimiter.infiniteLoop(e);
        }
        finally {
          RecursionLimiter.endExecution();
        }

        return result;
      }
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (name.length() > 0 && name.equals(key)) {
      return getProperty(key);
    }
    return super.getLocalizedProperty(key);
  }

  /**
   * Evaluate the calculated property. Do not call Decorator.reportDataError as this will probably
   * cause an infinite reporting loop.
   *
   * @return value
   */
  protected String evaluate() {
    try {
      return expression.evaluate(Decorator.getOutermost(this));
    }
    catch (ExpressionException e) {
      ErrorDialog.dataWarning(new BadDataReport(Resources.getString("Error.expression_error"),
        piece.getProperty(BasicPiece.BASIC_NAME) + "-Calculated Property[" + name + "]=" + getExpression() + ", Error=" + e.getError(), e)); // NON-NLS
      return "";
    }
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof CalculatedProperty)) return false;
    final CalculatedProperty c = (CalculatedProperty) o;
    if (! Objects.equals(name, c.name)) return false;
    return Objects.equals(expression, c.expression);
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * Trait Editor implementation
   *
   */
  public static class Ed implements PieceEditor {

    protected StringConfigurer nameConfig;
    protected BeanShellExpressionConfigurer expressionConfig;
    protected StringConfigurer defaultValueConfig;
    protected TraitConfigPanel box;
    private final StringConfigurer descConfig;

    public Ed(CalculatedProperty piece) {

      box = new TraitConfigPanel();

      descConfig = new StringConfigurer(piece.description);
      descConfig.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", descConfig);

      nameConfig = new StringConfigurer(piece.name);
      box.add("Editor.CalculatedProperty.property_name", nameConfig);

      expressionConfig = new BeanShellExpressionConfigurer(piece.getExpression(), Decorator.getOutermost(piece));
      box.add("Editor.CalculatedProperty.expression", expressionConfig);

    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameConfig.getValueString())
        .append(expressionConfig.getValueString())
        .append(descConfig.getValueString());
      return ID + se.getValue();
    }
  }

  // Implement Loopable
  @Override
  public String getComponentName() {
    // Use inner name to prevent recursive looping when reporting errors.
    return piece.getName();
  }

  @Override
  public String getComponentTypeName() {
    return getDescription();
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    l.add(name);
    return l;
  }

}
