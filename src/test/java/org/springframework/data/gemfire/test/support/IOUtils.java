/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.test.support;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The {@link IOUtils} class is an abstract utility class for working with IO operations.
 *
 * @author John Blum
 * @see java.io.Closeable
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public abstract class IOUtils {

	protected static final Logger log = Logger.getLogger(IOUtils.class.getName());

	/* (non-Javadoc) */
	public static boolean close(Closeable closeable) {

		if (closeable != null) {
			try {
				closeable.close();
				return true;
			}
			catch (IOException cause) {
				if (log.isLoggable(Level.FINE)) {
					log.fine(String.format("Failed to close the Closeable object (%1$s) due to an I/O error:%n%2$s",
						closeable, ThrowableUtils.toString(cause)));
				}
			}
		}

		return false;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	public static <T> T deserializeObject(byte[] objectBytes) throws IOException, ClassNotFoundException {

		ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(objectBytes);

		ObjectInputStream objectInputStream = null;

		try {
			objectInputStream = new ObjectInputStream(byteArrayInputStream);

			return (T) objectInputStream.readObject();
		}
		finally {
			IOUtils.close(objectInputStream);
		}
	}

	/* (non-Javadoc) */
	public static byte[] serializeObject(Serializable obj) throws IOException {

		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();

		ObjectOutputStream objectOutputStream = null;

		try {
			objectOutputStream = new ObjectOutputStream(byteArrayOutputStream);
			objectOutputStream.writeObject(obj);
			objectOutputStream.flush();

			return byteArrayOutputStream.toByteArray();
		}
		finally {
			IOUtils.close(objectOutputStream);
		}
	}
}
