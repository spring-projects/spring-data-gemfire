/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.test.support;

import java.io.Closeable;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The IOUtils class is an utility class working with IO operations.
 *
 * @author John Blum
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public abstract class IOUtils {

	private static final Logger log = Logger.getLogger(IOUtils.class.getName());

	public static boolean close(final Closeable closeable) {
		if (closeable != null) {
			try {
				closeable.close();
				return true;
			}
			catch (IOException ignore) {
				if (log.isLoggable(Level.FINE)) {
					log.fine(String.format("Failed to close Closeable object (%1$s) due to I/O error:%n%2$s",
						closeable, ThrowableUtils.toString(ignore)));
				}

			}
		}

		return false;
	}
}
