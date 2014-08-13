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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.springframework.util.Assert;

/**
 * The FileUtils class is a utility class for processing files, working with java.io.File objects.
 *
 * @author John Blum
 * @see java.io.File
 * @see org.springframework.data.gemfire.test.support.IOUtils
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public abstract class FileUtils extends IOUtils {

	public static final String LINE_SEPARATOR = System.getProperty("line.separator");

	public static String read(final File file) throws IOException {
		Assert.isTrue(file != null && file.isFile(), String.format(
			"The File reference (%1$s) from which to read the contents is not a valid file!", file));

		assert file != null;

		BufferedReader fileReader = new BufferedReader(new FileReader(file));

		try {
			StringBuilder buffer = new StringBuilder();

			for (String line = fileReader.readLine(); line != null; line = fileReader.readLine()) {
				buffer.append(line);
				buffer.append(LINE_SEPARATOR);
			}

			return buffer.toString().trim();
		}
		finally {
			close(fileReader);
		}
	}

}
