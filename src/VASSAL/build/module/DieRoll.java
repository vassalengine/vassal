/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Brent Easton
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


/**
 * @author Brent Easton
 *
 * Describes a single roll of one or more identical dice.
 * For use with internet dice rollers
 */
public class DieRoll {

  private String description = ""; //$NON-NLS-1$
  private int numSides;
  private int plus;
  private boolean reportTotal;
  private int[] result;

  public DieRoll(String d, int dice, int sides, int add, boolean r) {
    setDescription(d);
    setNumDice(dice);
    setNumSides(sides);
    setPlus(add);
    setReportTotal(r);
    result = new int[dice];
  }


  public DieRoll(String d, int dice, int sides, int add) {
    this(d, dice, sides, add, false);
  }

  public DieRoll(String d, int dice, int sides) {
    this(d, dice, sides, 0);
  }

  public int getResult(int pos) {
    return result[pos];
  }

  public void setResult(int pos, int res) {
    result[pos] = res;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public int getNumDice() {
    return result.length;
  }

  public void setNumDice(int numDice) {
    result = new int[numDice];
  }

  public int getNumSides() {
    return numSides;
  }

  public void setNumSides(int numSides) {
    this.numSides = numSides;
  }

  public int getPlus() {
    return plus;
  }

  public void setPlus(int plus) {
    this.plus = plus;
  }

  public boolean isReportTotal() {
    return reportTotal;
  }

  public void setReportTotal(boolean reportTotal) {
    this.reportTotal = reportTotal;
  }
}