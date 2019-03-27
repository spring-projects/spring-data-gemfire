/*
 * Copyright 2010-2013 the original author or authors.
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The FileUtils class is a utility class for processing files, working with java.io.File objects.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.io.FileReader
 * @see java.io.FileWriter
 * @see org.springframework.data.gemfire.test.support.IOUtils
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public abstract class FileUtils extends IOUtils {

	public static final String LINE_SEPARATOR = System.getProperty("line.separator");

	public static boolean isDirectory(final File path) {
		return (path != null && path.isDirectory());
	}

	public static boolean isFile(final File path) {
		return (path != null && path.isFile());
	}

	// TODO refactor and perhaps replace with org.springframework.util.FileCopyUtils.copyToString(:Reader)
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

	// TODO refactor and perhaps replace with org.springframework.util.FileCopyUtils.copy(:String, :Writer)
	public static void write(final File file, final String contents) throws IOException {
		Assert.notNull(file, "The File to write to must not be null!");
		Assert.isTrue(StringUtils.hasText(contents), "The 'contents' of the file cannot be null or empty!");

		BufferedWriter fileWriter = null;

		try {
			fileWriter = new BufferedWriter(new FileWriter(file));
			fileWriter.write(contents);
			fileWriter.flush();
		}
		finally {
			IOUtils.close(fileWriter);
		}
	}

}
