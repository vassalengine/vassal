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

public class SortParameter {
  private String sortProperty = "";
  private boolean ascendingSort = true;

  public SortParameter() {

  }

  public SortParameter(String code) {
    this();
    decode(code);
  }

  public SortParameter(boolean ascending, String property) {
    setSortProperty(property);
    setAscendingSort(ascending);
  }

  public String encode() {
    return ascendingSort + "|" + sortProperty;
  }

  @Override
  public String toString() {
    return encode();
  }

  public void decode(String code) {
    if (code == null) {
      setAscendingSort(true);
      setSortProperty("");
      return;
    }

    if (code.startsWith("true|")) {
      setAscendingSort(true);
      setSortProperty(code.length() > 5 ? code.substring(5) : "");
      return;
    }
    else if (code.startsWith("false|")) {
      setAscendingSort(false);
      setSortProperty(code.length() > 6 ? code.substring(6) : "");
      return;
    }

    setAscendingSort(true);
    setSortProperty("");

  }

  public void setValue(SortParameter param) {
    setAscendingSort(param.isAscendingSort());
    setSortProperty(param.getSortProperty());
  }

  public String getSortProperty() {
    return sortProperty;
  }

  public void setSortProperty(String sortProperty) {
    this.sortProperty = sortProperty;
  }

  public boolean isAscendingSort() {
    return ascendingSort;
  }

  public void setAscendingSort(boolean ascendingSort) {
    this.ascendingSort = ascendingSort;
  }
}
