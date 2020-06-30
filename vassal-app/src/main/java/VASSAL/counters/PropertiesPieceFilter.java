/*
 *
 * Copyright (c) 2005-2012 by Rodney Kinney, Brent Easton
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

import java.util.regex.Pattern;

import VASSAL.script.expression.BeanShellExpression;
import VASSAL.script.expression.FormattedStringExpression;

/**
 * Accepts pieces based on whether the piece has properties that
 * match a given set of conditions
 */
public class PropertiesPieceFilter {

  private static final Pattern[] CONDITIONS = new Pattern[]{Pattern.compile("!="),
                                                            Pattern.compile("<="),
                                                            Pattern.compile(">="),
                                                            Pattern.compile(">"),
                                                            Pattern.compile("<"),
                                                            Pattern.compile("=~"),
                                                            Pattern.compile("="),
                                                            Pattern.compile("!~")};

  private static final Pattern AND = Pattern.compile("&&");
  private static final Pattern OR = Pattern.compile("\\|\\|");

  private static PieceFilter ACCEPT_ALL = new PieceFilter() {
    @Override
    public boolean accept(GamePiece piece) {
      return true;
    }
  };

  /**
   * Return a PieceFilter parsed from a boolean expression such as
   * prop1 = value1 && prop2 = value2 || prop3 = value3
   * @param expression
   * @return
   */
  public static PieceFilter parse(String expression) {
    if (expression == null
        || expression.length() == 0) {
      return ACCEPT_ALL;
    }
    String[] s = OR.split(expression);
    PieceFilter f = null;
    if (s.length > 1) {
      f = parse(s[0]);
      for (int i = 1; i < s.length; ++i) {
        f = new BooleanOrPieceFilter(f, parse(s[i]));
      }
    }
    else {
      s = AND.split(expression);
      if (s.length > 1) {
        f = parse(s[0]);
        for (int i = 1; i < s.length; ++i) {
          f = new BooleanAndPieceFilter(f, parse(s[i]));
        }
      }
      else {
        for (int i = 0; i < CONDITIONS.length && f == null; i++) {
          if (expression.contains(CONDITIONS[i].pattern())) {
            s = CONDITIONS[i].split(expression);
            String name = "";
            String value = "";
            if (s.length > 0) {
              name = s[0].trim();
              if (s.length > 1) {
                value = s[1].trim();
              }
              switch (i) {
                case 0:
                  f = new NE(name, value);
                  break;
                case 1:
                  f = new LE(name, value);
                  break;
                case 2:
                  f = new GE(name, value);
                  break;
                case 3:
                  f = new GT(name, value);
                  break;
                case 4:
                  f = new LT(name, value);
                  break;
                case 5:
                  f = new MATCH(name, value);
                  break;
                case 6:
                  f = new EQ(name, value);
                  break;
                case 7:
                  f = new NOT_MATCH(name,value);
              }
            }
            break;
          }
        }
        if (f == null) {
          f = ACCEPT_ALL;
        }
      }
    }
    return f;
  }

  public static String toBeanShellString(String s) {
    return toBeanShellString(parse(s));
  }

  public static String toBeanShellString(PieceFilter f) {

    if (f instanceof BooleanAndPieceFilter) {
      final BooleanAndPieceFilter and = (BooleanAndPieceFilter) f;
      return "("+toBeanShellString(and.getFilter1()) + ") && (" + toBeanShellString(and.getFilter2()) + ")";
    }
    else if (f instanceof BooleanOrPieceFilter) {
      final BooleanOrPieceFilter or = (BooleanOrPieceFilter) f;
      return "("+toBeanShellString(or.getFilter1()) + ") || (" + toBeanShellString(or.getFilter2()) + ")";
    }
    else if (f instanceof ComparisonFilter) {
      return  ((ComparisonFilter) f).toBeanShellString();
    }
    return "";

  }

  private static abstract class ComparisonFilter implements PieceFilter {
    protected String name;
    protected String value;
    protected Object alternate;

    public ComparisonFilter(String name, String value) {
      this.name = name;
      this.value = value;
      if ("true".equals(value)) {
        alternate = Boolean.TRUE;
      }
      else if ("false".equals(value)) {
        alternate = Boolean.FALSE;
      }
    }

    protected int compareTo(GamePiece piece) {
      String property = String.valueOf(piece.getProperty(name));
      try {
        return Integer.valueOf(property).compareTo(Integer.valueOf(value));
      }
      catch (NumberFormatException e) {
        // If both properties are not numbers, compare alphabetically
        return property.compareTo(value);
      }
    }

    public abstract String toBeanShellString();

    protected String toBeanShellName() {
      if (name.indexOf('$') >= 0) {
        return "GetProperty("+new FormattedStringExpression(name).toBeanShellString() + ")";
      }
      else {
        return BeanShellExpression.convertProperty(name);
      }
    }

    protected String toBeanShellValue() {
      return new FormattedStringExpression(value).toBeanShellString();
    }

  }

  private static class EQ extends ComparisonFilter {
    public EQ(String name, String value) {
      super(name, value);
    }

    @Override
    public boolean accept(GamePiece piece) {
      String property = String.valueOf(piece.getProperty(name));
      boolean retVal = value.equals(property);
      if (alternate != null) {
        retVal = retVal || alternate.equals(property);
      }
      return retVal;
    }

    public String toString() {
      return "PropertiesPieceFilter["+name+"=="+value+"]";
    }

    @Override
    public String toBeanShellString() {
      return toBeanShellName() + "==" + toBeanShellValue();
    }

  }

  private static class NE extends ComparisonFilter {
    public NE(String name, String value) {
      super(name, value);
    }

    @Override
    public boolean accept(GamePiece piece) {
      String property = String.valueOf(piece.getProperty(name));
      boolean retVal = !value.equals(property);
      if (alternate != null) {
        retVal = retVal && !alternate.equals(property);
      }
      return retVal;
    }
    public String toString() {
      return "PropertiesPieceFilter["+name+"!="+value+"]";
    }

    @Override
    public String toBeanShellString() {
      return toBeanShellName() + "!=" + toBeanShellValue();
    }
  }

  private static class LT extends ComparisonFilter {
    public LT(String name, String value) {
      super(name, value);
    }

    @Override
    public boolean accept(GamePiece piece) {
      return compareTo(piece) < 0;
    }

    public String toString() {
      return "PropertiesPieceFilter["+name+"<"+value+"]";
    }

    @Override
    public String toBeanShellString() {
      return toBeanShellName() + "<" + toBeanShellValue();
    }
  }

  private static class LE extends ComparisonFilter {
    public LE(String name, String value) {
      super(name, value);
    }

    @Override
    public boolean accept(GamePiece piece) {
      return compareTo(piece) <= 0;
    }

    public String toString() {
      return "PropertiesPieceFilter["+name+"<="+value+"]";
    }

    @Override
    public String toBeanShellString() {
      return toBeanShellName() + "<=" + toBeanShellValue();
    }
  }

  private static class GT extends ComparisonFilter {
    public GT(String name, String value) {
      super(name, value);
    }

    @Override
    public boolean accept(GamePiece piece) {
      return compareTo(piece) > 0;
    }

    public String toString() {
      return "PropertiesPieceFilter["+name+">"+value+"]";
    }

    @Override
    public String toBeanShellString() {
      return toBeanShellName() + ">" + toBeanShellValue();
    }
  }

  private static class GE extends ComparisonFilter {
    public GE(String name, String value) {
      super(name, value);
    }

    @Override
    public boolean accept(GamePiece piece) {
      return compareTo(piece) >= 0;
    }

    public String toString() {
      return "PropertiesPieceFilter["+name+">="+value+"]";
    }

    @Override
    public String toBeanShellString() {
      return toBeanShellName() + ">=" + toBeanShellValue();
    }
  }

  private static class MATCH extends ComparisonFilter {
    public MATCH(String name, String value) {
      super(name, value);
    }

    @Override
    public boolean accept(GamePiece piece) {
      String property = String.valueOf(piece.getProperty(name));
      return Pattern.matches(value, property);
    }

    public String toString() {
      return "PropertiesPieceFilter["+name+"~"+value+"]";
    }

    @Override
    public String toBeanShellString() {
      return toBeanShellName() + "=~" + toBeanShellValue();
    }
  }

  private static class NOT_MATCH extends MATCH {
    public NOT_MATCH(String name, String value) {
      super(name, value);
    }

    @Override
    public boolean accept(GamePiece piece) {
      return !super.accept(piece);
    }

    public String toString() {
      return "PropertiesPieceFilter["+name+"!~"+value+"]";
    }

    @Override
    public String toBeanShellString() {
      return toBeanShellName() + "!~" + toBeanShellValue();
    }
  }
}
