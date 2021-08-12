/*
 *
 * Copyright (c) 2021 by The VASSAL Development Team
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
package VASSAL.build.module.map.deck;

import VASSAL.tools.SequenceEncoder;

public class SortParameter {
  private String sortProperty = "";
  private boolean descendingSort = false;
  private boolean numericSort;

  public SortParameter() {

  }

  public SortParameter(String code) {
    this();
    decode(code);
  }

  public SortParameter(String property, boolean descending, boolean numeric) {
    setSortProperty(property);
    setDescendingSort(descending);
    setNumericSort(numeric);
  }

  public String encode() {
    final SequenceEncoder se = new SequenceEncoder('|');
    se.append(getSortProperty()).append(isDescendingSort()).append(isNumericSort());
    return se.getValue();
  }

  @Override
  public String toString() {
    return encode();
  }

  public void decode(String code) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, '|');
    setSortProperty(sd.nextToken(""));
    setDescendingSort(sd.nextBoolean(false));
    setNumericSort(sd.nextBoolean(false));
  }

  public void setValue(SortParameter param) {
    setDescendingSort(param.isDescendingSort());
    setSortProperty(param.getSortProperty());
    setNumericSort(param.isNumericSort());
  }

  public String getSortProperty() {
    return sortProperty;
  }

  public void setSortProperty(String sortProperty) {
    this.sortProperty = sortProperty;
  }

  public boolean isDescendingSort() {
    return descendingSort;
  }

  public void setDescendingSort(boolean descendingSort) {
    this.descendingSort = descendingSort;
  }

  public boolean isNumericSort() {
    return numericSort;
  }

  public void setNumericSort(boolean numericSort) {
    this.numericSort = numericSort;
  }
}
