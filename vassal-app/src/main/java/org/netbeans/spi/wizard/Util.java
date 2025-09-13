/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.spi.wizard;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Tim Boudreau
 */
final class Util {
    private Util(){}
    
    /**
     * Get an array of step ids from an array of WizardPages
     */
    static String[] getSteps(WizardPage[] pages) {
        String[] result = new String[pages.length];

        Set uniqueNames = new HashSet(pages.length);
        for (int i = 0; i < pages.length; i++) {
            result[i] = pages[i].id();
            if (result[i] == null || uniqueNames.contains(result[i])) {
                result[i] = uniquify (getIDFromStaticMethod(pages[i].getClass()), 
                        uniqueNames);
                pages[i].id = result[i];
            }
            uniqueNames.add (result[i]);
        }
        return result;
    }
    
    static String uniquify (String s, Set /* <String> */ used) {
        String test = s;
        if (test != null) {
            int ix = 0;
            while (used.contains(test)) {
                test = s + "_" + ix++;
            }
        }
        return test;
    }

    /**
     * Get an array of descriptions from an array of WizardPages
     */
    static String[] getDescriptions(WizardPage[] pages) {
        String[] result = new String[pages.length];

        for (int i = 0; i < pages.length; i++) {
            result[i] = pages[i].description();
            if (result[i] == null) {
                result[i] = getDescriptionFromStaticMethod (pages[i].getClass());
            }
        }

        return result;
    }

    static String getIDFromStaticMethod (Class clazz) {
        // System.err.println("GetID by method for " + clazz);
        String result = null;
        try {
            Method m = clazz.getDeclaredMethod("getStep", new Class[] {});
            // assert m.getReturnType() == String.class;
            result = (String) m.invoke(clazz, (Object[]) null);
            if (result == null) {
                throw new NullPointerException ("getStep may not return null");
            }
        } catch (Exception ex) {
            //do nothing
        }
        return result == null ? clazz.getName() : result;
    }

    /**
     * Get an array of steps by looking for a static method getID() on each
     * class object passed
     */
    static String[] getSteps(Class[] pages) {
        if (pages == null) {
            throw new NullPointerException("Null array of classes"); //NOI18N
        }

        String[] result = new String[pages.length];

        Set used = new HashSet (pages.length);
        for (int i = 0; i < pages.length; i++) {
            if (pages[i] == null) {
                throw new NullPointerException("Null at " + i + " in array " + //NOI18N
                        "of panel classes"); //NOI18N
            }

            if (!WizardPage.class.isAssignableFrom(pages[i])) {
                throw new IllegalArgumentException(pages[i] +
                        " is not a subclass of WizardPage"); //NOI18N
            }
            result[i] = uniquify (getIDFromStaticMethod (pages[i]), used);
            if (result[i] == null) {
                result[i] = pages[i].getName();
            }
        }
        // System.err.println("Returning " + Arrays.asList(result));
        return result;
    }

//    /** Determine if a default constructor is present for a class */
//    private static boolean hasDefaultConstructor (Class clazz) {
//        try {
//            Constructor c = clazz.getConstructor(new Class[0]);
//            return c != null;
//        } catch (Exception e) {
//            return false;
//        }
//    }

    /**
     * Get an array of descriptions by looking for the static method
     * getDescription() on each passed class object
     */
    static String[] getDescriptions(Class[] pages) {
        String[] result = new String[pages.length];

        for (int i = 0; i < pages.length; i++) {
            result[i] = getDescriptionFromStaticMethod(pages[i]);
        }

        return result;
    }

    static String getDescriptionFromStaticMethod(Class clazz) {
        String result = null;
        Method m;
        try {
            m = clazz.getDeclaredMethod("getDescription", (Class[]) null); //NOI18N
        } catch (Exception e) {
            throw new IllegalArgumentException("Could not find or access " + //NOI18N
                    "public static String " + clazz.getName() +  //NOI18N
                    ".getDescription() - make sure it exists"); //NOI18N
        }

        if (m.getReturnType() != String.class) {
            throw new IllegalArgumentException("getStep has wrong " //NOI18N
                    + " return type: " + m.getReturnType() + " on " + //NOI18N
                    clazz);
        }

        if (!Modifier.isStatic(m.getModifiers())) {
            throw new IllegalArgumentException("getStep is not " + //NOI18N
                    "static on " + clazz); //NOI18N
        }

        try {
            m.setAccessible(true);
            result= (String) m.invoke(null, (Object[]) null);
        } catch (InvocationTargetException ite) {
            throw new IllegalArgumentException("Could not invoke " + //NOI18N
                    "public static String " + clazz.getName() +  //NOI18N
                    ".getDescription() - make sure it exists."); //NOI18N
        } catch (IllegalAccessException iae) {
            throw new IllegalArgumentException("Could not invoke " + //NOI18N
                    "public static String " + clazz.getName() +  //NOI18N
                    ".getDescription() - make sure it exists."); //NOI18N
        }
        return result;
    }
}
