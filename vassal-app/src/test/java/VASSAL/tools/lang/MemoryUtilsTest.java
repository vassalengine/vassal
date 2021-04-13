/*
 *
 * Copyright (c) 2007 by Joel Uckelman
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

package VASSAL.tools.lang;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

import static org.junit.jupiter.api.Assertions.*;

public class MemoryUtilsTest {
  @Test
  @EnabledOnOs({ OS.LINUX })
  public void testGetPhysicalMemoryLinux() throws IOException {

    // get the total RAM from the system, in kB
    final Process p = Runtime.getRuntime().exec(new String[] {
      "sh",
      "-c",
      "grep '^MemTotal:' /proc/meminfo | sed 's/[^0-9]//g'"
    });

    final BufferedReader r =
      new BufferedReader(new InputStreamReader(p.getInputStream()));

    final int eRAM = Integer.parseInt(r.readLine());
    r.close();

    // check that it's correct
    assertEquals(eRAM, MemoryUtils.getPhysicalMemory() >> 10);
  }

  // FIXME: how to get RAM on MacOS?
  @Test
  @EnabledOnOs({ OS.MAC })
  public void testGetPhysicalMemoryMacOS() {
    assertTrue(true);
  }

  // FIXME: how to get RAM on Windows?
  @Test
  @EnabledOnOs({ OS.WINDOWS })
  public void testGetPhysicalMemoryWindows() {
    assertTrue(true);
  }
}
