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

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.FileCopyUtils;

/**
 * The ZipUtils class is an abstract utility class for working with JAR and ZIP archives.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.util.zip.ZipFile
 * @since 1.5.0
 */
public abstract class ZipUtils {

	public static void unzip(final Resource zipResource, final File directory) throws IOException {
		Assert.notNull(zipResource, "The ZIP Resource must not be null!");

		Assert.isTrue(directory != null && directory.isDirectory(), String.format(
			"The file system pathname (%1$s) is not a valid directory!", directory));

		ZipFile zipFile = new ZipFile(zipResource.getFile(), ZipFile.OPEN_READ);

		for (ZipEntry entry : CollectionUtils.iterable(zipFile.entries())) {
			if (entry.isDirectory()) {
				new File(directory, entry.getName()).mkdirs();
			}
			else {
				DataInputStream entryInputStream = new DataInputStream(zipFile.getInputStream(entry));

				DataOutputStream entryOutputStream = new DataOutputStream(new FileOutputStream(
					new File(directory, entry.getName())));

				try {
					FileCopyUtils.copy(entryInputStream, entryOutputStream);
				}
				finally {
					IOUtils.close(entryInputStream);
					IOUtils.close(entryOutputStream);
				}
			}
		}
	}
}
