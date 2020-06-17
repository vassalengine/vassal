package VASSAL.build.module.noteswindow;

/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

/**
 * A text message with an owner
 */
public class PrivateText {
  private String owner;
  private String text;

  public PrivateText(String owner, String text) {
    this.owner = owner;
    this.text = text;
  }

  public String getOwner() {
    return owner;
  }

  public String getText() {
    return text;
  }

  /**
   * Two PrivateTexts with the same owner are considered equal
   * @param o
   * @return
   */
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof PrivateText)) return false;

    final PrivateText privateText = (PrivateText) o;

    if (!owner.equals(privateText.owner)) return false;

    return true;
  }

  public int hashCode() {
    return owner.hashCode();
  }
}
