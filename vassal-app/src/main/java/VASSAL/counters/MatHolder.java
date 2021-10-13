/*
 *
 * Copyright (c) 2021 by The VASSAL Development team
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

import java.util.ArrayList;
import java.util.List;

/**
 * A utility class for holding and manipulating Mats and their Cargo
 */
public class MatHolder {
  private final GamePiece matPiece;
  private final Mat mat;
  private List<GamePiece> cargo = new ArrayList<>();

  public MatHolder(GamePiece piece) {
    this.matPiece = piece;
    mat = (Mat) Decorator.getDecorator(piece, Mat.class);
  }

  /**
   * Remove any cargo on this mat from the supplied allCargo list
   * and add to the MatHolders cargo list
   *
   * @param allCargo
   */
  public void grabCargo(List<GamePiece> allCargo) {
    for (final GamePiece cargoPiece : mat.getContents()) {
      cargo.add(cargoPiece);
      allCargo.remove(cargoPiece);
    }

  }

  public GamePiece getMatPiece() {
    return matPiece;
  }

  public Mat getMat() {
    return mat;
  }

  public List<GamePiece> getCargo() {
    return cargo;
  }

  public void setCargo(List<GamePiece> cargo) {
    this.cargo = cargo;
  }
}