/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.chat;

/**
 * Immutable PlayerStatus class with flags indicating "looking for a game" and "away from keyboard" and a String profile
 * 
 * @author rkinney
 * 
 */
public class SimpleStatus implements PlayerStatus {
  private boolean looking;
  private boolean away;
  private String profile;

  public SimpleStatus() {
    this(false, false, ""); //$NON-NLS-1$
  }

  public SimpleStatus(boolean looking, boolean away, String profile) {
    this.looking = looking;
    this.away = away;
    this.profile = profile;
  }

  public boolean isAway() {
    return away;
  }

  public boolean isLooking() {
    return looking;
  }

  public String getProfile() {
    return profile;
  }

}
