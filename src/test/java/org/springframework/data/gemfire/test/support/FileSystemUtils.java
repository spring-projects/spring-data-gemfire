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

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.gemstone.gemfire.management.internal.cli.util.spring.Assert;

/**
 * The FileSystemUtils class is a utility class encapsulating functionality to process file system directories
 * and files collectively.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.springframework.data.gemfire.test.support.FileUtils
 * @see org.springframework.data.gemfire.test.support.IOUtils
 * @since 1.5.0
 */
@SuppressWarnings("unused")
public abstract class FileSystemUtils extends FileUtils {

	public static final File JAVA_HOME = new File(System.getProperty("java.home"));
	public static final File JAVA_EXE = new File(new File(JAVA_HOME, "bin"), "java");
	public static final File USER_HOME = new File(System.getProperty("user.home"));
	public static final File WORKING_DIRECTORY = new File(System.getProperty("user.dir"));

	public static boolean deleteRecursive(final File path) {
		assert path != null;

		boolean success = true;

		if (path.isDirectory()) {
			for (File file : safeListFiles(path)) {
				success &= deleteRecursive(file);
			}
		}

		return (path.delete() && success);
	}

	public static File getRootRelativeToWorkingDirectoryOrPath(final File path) {
		File localPath = path;

		if (isDirectory(localPath)) {
			while (localPath != null && !WORKING_DIRECTORY.equals(localPath.getParentFile())) {
				localPath = localPath.getParentFile();
			}
		}

		return (localPath != null ? localPath : path);
	}

	public static File[] listFiles(final File directory, final FileFilter fileFilter) {
		Assert.isTrue(directory != null && directory.isDirectory(), String.format(
			"File (%1$s) does not refer to a valid directory", directory));

		List<File> results = new ArrayList<File>();

		for (File file : safeListFiles(directory, fileFilter)) {
			if (file.isDirectory()) {
				results.addAll(Arrays.asList(listFiles(file, fileFilter)));
			}
			else {
				results.add(file);
			}
		}

		return results.toArray(new File[results.size()]);
	}

	public static File[] safeListFiles(final File directory) {
		return safeListFiles(directory, AllFiles.INSTANCE);
	}

	public static File[] safeListFiles(final File directory, final FileFilter fileFilter) {
		File[] files = (directory != null ? directory.listFiles(fileFilter) : new File[0]);
		return (files != null ? files : new File[0]);
	}

	public static final class AllFiles implements FileFilter {

		public static final AllFiles INSTANCE = new AllFiles();

		@Override
		public boolean accept(final File pathname) {
			return true;
		}
	}

	public static final class FileOnlyFilter implements FileFilter {

		public static final FileOnlyFilter INSTANCE = new FileOnlyFilter();

		@Override
		public boolean accept(final File pathname) {
			return (pathname != null && pathname.isFile());
		}
	}

	public static final class DirectoryOnlyFilter implements FileFilter {

		public static final DirectoryOnlyFilter INSTANCE = new DirectoryOnlyFilter();

		@Override
		public boolean accept(final File pathname) {
			return (pathname != null && pathname.isDirectory());
		}
	}

}
