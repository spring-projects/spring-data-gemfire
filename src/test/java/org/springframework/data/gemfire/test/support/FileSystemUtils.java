/*
 * Copyright 2010-2020 the original author or authors.
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

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.springframework.util.Assert;

/**
 * The {@link FileSystemUtils} class is a utility class encapsulating functionality to process
 * file system directories and files collectively.
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
	public static final File TEMPORARY_DIRECTORY = new File(System.getProperty("java.io.tmpdir"));
	public static final File USER_HOME = new File(System.getProperty("user.home"));
	public static final File WORKING_DIRECTORY = new File(System.getProperty("user.dir"));

	public static final File[] NO_FILES = new File[0];

	/* (non-Javadoc) */
	public static boolean deleteRecursive(File path) {
		return deleteRecursive(path, AllFilesFilter.INSTANCE);
	}

	/* (non-Javadoc) */
	public static boolean deleteRecursive(File path, FileFilter fileFilter) {
		boolean success = true;

		if (isDirectory(path)) {
			for (File file : safeListFiles(path, fileFilter)) {
				success &= deleteRecursive(file);
			}
		}

		return ((path == null || path.delete()) && success);
	}

	// returns sub-directory just below working directory
	public static File getRootRelativeToWorkingDirectoryOrPath(File path) {
		File localPath = path;

		if (isDirectory(localPath)) {
			while (localPath != null && !WORKING_DIRECTORY.equals(localPath.getParentFile())) {
				localPath = localPath.getParentFile();
			}
		}

		return (localPath != null ? localPath : path);
	}

	/* (non-Javadoc) */
	public static File[] listFiles(File directory, FileFilter fileFilter) {
		Assert.isTrue(isDirectory(directory), String.format(
			"File [%s] does not refer to a valid directory", directory));

		List<File> results = new ArrayList<File>();

		for (File file : safeListFiles(directory, fileFilter)) {
			if (isDirectory(file)) {
				results.addAll(Arrays.asList(listFiles(file, fileFilter)));
			}
			else {
				results.add(file);
			}
		}

		return results.toArray(new File[results.size()]);
	}

	/* (non-Javadoc) */
	public static File[] safeListFiles(File directory) {
		return safeListFiles(directory, AllFilesFilter.INSTANCE);
	}

	/* (non-Javadoc) */
	public static File[] safeListFiles(File directory, FileFilter fileFilter) {
		FileFilter resolvedFileFilter = (fileFilter != null ? fileFilter : AllFilesFilter.INSTANCE);
		File[] files = (isDirectory(directory) ? directory.listFiles(resolvedFileFilter) : null);
		return (files != null ? files : NO_FILES);
	}

	public static class AllFilesFilter implements FileFilter {

		public static final AllFilesFilter INSTANCE = new AllFilesFilter();

		@Override
		public boolean accept(File pathname) {
			return true;
		}
	}

	public static class CompositeFileFilter implements FileFilter {

		private final FileFilter fileFilterOne;
		private final FileFilter fileFilterTwo;

		private final LogicalOperator logicalOperator;

		private CompositeFileFilter(FileFilter fileFilterOne, LogicalOperator operator, FileFilter fileFilterTwo) {
			this.fileFilterOne = fileFilterOne;
			this.logicalOperator = operator;
			this.fileFilterTwo = fileFilterTwo;
		}

		protected static FileFilter compose(FileFilter fileFilterOne, LogicalOperator operator, FileFilter fileFilterTwo) {
			return (fileFilterOne == null ? fileFilterTwo : (fileFilterTwo == null ? fileFilterOne
				: new CompositeFileFilter(fileFilterOne, operator, fileFilterTwo)));
		}

		public static FileFilter and(FileFilter... fileFilters) {
			return and(Arrays.asList(nullSafeArray(fileFilters, FileFilter.class)));
		}

		public static FileFilter and(Iterable<FileFilter> fileFilters) {
			FileFilter current = null;

			for (FileFilter fileFilter : nullSafeIterable(fileFilters)) {
				current = compose(current, LogicalOperator.AND, fileFilter);
			}

			return current;
		}

		public static FileFilter or(FileFilter... fileFilters) {
			return or(Arrays.asList(nullSafeArray(fileFilters, FileFilter.class)));
		}

		public static FileFilter or(Iterable<FileFilter> fileFilters) {
			FileFilter current = null;

			for (FileFilter fileFilter : nullSafeIterable(fileFilters)) {
				current = compose(current, LogicalOperator.OR, fileFilter);
			}

			return current;
		}

		@Override
		public boolean accept(File pathname) {
			switch (this.logicalOperator) {
				case AND:
					return (fileFilterOne.accept(pathname) && fileFilterTwo.accept(pathname));
				case OR:
					return (fileFilterOne.accept(pathname) || fileFilterTwo.accept(pathname));
				default:
					throw new UnsupportedOperationException(String.format(
						"Logical operator [%s] is unsupported", this.logicalOperator));
			}
		}

		enum LogicalOperator {
			AND, OR;
		}
	}

	public static class DirectoryOnlyFilter implements FileFilter {

		public static final DirectoryOnlyFilter INSTANCE = new DirectoryOnlyFilter();

		@Override
		public boolean accept(File pathname) {
			return isDirectory(pathname);
		}
	}

	public static final class FileExtensionFilter extends FileOnlyFilter {

		private final String fileExtension;

		public static FileExtensionFilter newFileExtensionFilter(String fileExtension) {
			return new FileExtensionFilter(fileExtension);
		}

		public FileExtensionFilter(String fileExtension) {
			Assert.hasText(fileExtension, String.format("File extension [%s] must be specified", fileExtension));
			this.fileExtension = fileExtension;
		}

		@Override
		public boolean accept(File pathname) {
			return (super.accept(pathname) && pathname.getAbsolutePath().toLowerCase().endsWith(this.fileExtension));
		}
	}

	public static class FileOnlyFilter implements FileFilter {

		public static final FileOnlyFilter INSTANCE = new FileOnlyFilter();

		@Override
		public boolean accept(File pathname) {
			return isFile(pathname);
		}
	}

	public static class NegatingFileFilter implements FileFilter {

		private final FileFilter delegate;

		public static NegatingFileFilter newNegatingFileFilter(FileFilter delegate) {
			return new NegatingFileFilter(delegate);
		}

		public NegatingFileFilter(FileFilter delegate) {
			Assert.notNull(delegate, "FileFilter must not be null");
			this.delegate = delegate;
		}

		@Override
		public boolean accept(File pathname) {
			return !this.delegate.accept(pathname);
		}
	}
}
