/*  The contents of this file are subject to the terms of the Common Development
and Distribution License (the License). You may not use this file except in
compliance with the License.
    You can obtain a copy of the License at http://www.netbeans.org/cddl.html
or http://www.netbeans.org/cddl.txt.
    When distributing Covered Code, include this CDDL Header Notice in each file
and include the License file at http://www.netbeans.org/cddl.txt.
If applicable, add the following below the CDDL Header, with the fields
enclosed by brackets [] replaced by your own identifying information:
"Portions Copyrighted [year] [name of copyright owner]" */

/* Portions Copyrighted 2007 Rodney Kinney */

package org.netbeans.modules.wizard;

import org.netbeans.api.wizard.WizardDisplayer;
import VASSAL.i18n.Resources;

/**
 * We replace this class from the SwingLabs wizard project in order to plug in our own i18n mechanism
 *
 * @author Tim Boudreau, Rodney Kinney
 */
public final class NbBridge {
    private NbBridge() {}
    public static boolean inNetBeans() {
      return false;
    }

    public static WizardDisplayer getFactoryViaLookup() {
        return null;
    }

    public static String getString (String path, Class callerType, String key) {
      return Resources.getString("Wizard."+key);
    }

}
