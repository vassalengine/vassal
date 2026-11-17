/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.module.map.boardPicker.board;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import VASSAL.build.module.map.boardPicker.Board;

import java.awt.Point;

import org.junit.jupiter.api.Test;

public class RegionGridTest {
  /**
   * Cancelling the properties window of a newly added grid deletes it. If
   * the grid's own window is updated afterwards, the grid is removed again
   * although it has no container any more.
   */
  @Test
  public void removingAGridTwiceLeavesTheBoardAlone() {
    final Board board = mock(Board.class);
    final RegionGrid grid = new RegionGrid();
    grid.addTo(board);

    grid.removeFrom(board);
    grid.removeFrom(board);

    verify(board, times(1)).removeGrid(grid);
  }

  /**
   * The Define Regions window stays open when the grid it shows is deleted,
   * so a region can still be dragged after the grid has left its board.
   */
  @Test
  public void aRegionOfAGridWithoutAContainerCanStillBeMoved() {
    final RegionGrid grid = new RegionGrid();
    final Region region = new Region(new Point(10, 10));
    region.addTo(grid);

    region.move(5, -5, null);

    assertThat(region.getOrigin(), is(new Point(15, 5)));
  }
}
