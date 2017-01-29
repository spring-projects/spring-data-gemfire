/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.snapshot;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotServiceAdapterSupport;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.gemfire.test.support.FileSystemUtils;

/**
 * The SnapshotServiceFactoryBeanIntegrationTest class is a test suite of test cases testing the file archive handling
 * capabilities of the SnapshotServiceFactoryBean.SnapshotServiceAdpterSupport class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean.SnapshotServiceAdapterSupport
 * @since 1.7.0
 */
public class SnapshotServiceFactoryBeanIntegrationTest {

	SnapshotServiceAdapterSupport snapshotService = new TestSnapshotServiceAdapter();

	protected List<String> toFilenames(File... files) {
		List<String> filenames = new ArrayList<String>(files.length);

		for (File file : files) {
			filenames.add(file.getName());
		}

		return filenames;
	}

	@Test
	public void handleNonArchiveFileLocation() {
		File expectedFile = new File("/path/to/non-existing/snapshot/file.gfd");

		File[] files = snapshotService.handleFileLocation(expectedFile);

		assertThat(files, is(notNullValue()));
		assertThat(files.length, is(equalTo(1)));
		assertThat(files[0], is(equalTo(expectedFile)));
	}

	@Test
	public void handleArchiveFileLocation() throws Exception {
		File cacheSnapshotZipDirectory = null;

		try {
			File cacheSnapshotZip = new ClassPathResource("/cache_snapshot.zip").getFile();

			File[] actualSnapshots = snapshotService.handleFileLocation(cacheSnapshotZip);

			assertThat(actualSnapshots, is(notNullValue()));
			assertThat(actualSnapshots.length, is(equalTo(3)));
			assertThat(toFilenames(actualSnapshots).containsAll(Arrays.asList(
				"accounts.snapshot", "address.snapshot", "people.snapshot")), is(true));

			cacheSnapshotZipDirectory = new File(System.getProperty("java.io.tmpdir"),
				cacheSnapshotZip.getName().replaceAll("\\.", "-"));

			assertThat(cacheSnapshotZipDirectory.isDirectory(), is(true));
			assertThat(cacheSnapshotZipDirectory.listFiles(FileSystemUtils.FileOnlyFilter.INSTANCE),
				is(equalTo(actualSnapshots)));
		}
		finally {
			if (cacheSnapshotZipDirectory != null && cacheSnapshotZipDirectory.isDirectory()) {
				FileSystemUtils.deleteRecursive(cacheSnapshotZipDirectory);
			}
		}
	}

	protected static final class TestSnapshotServiceAdapter<K, V> extends SnapshotServiceAdapterSupport<K, V> {

		@Override
		protected File[] handleLocation(final SnapshotServiceFactoryBean.SnapshotMetadata<K, V> configuration) {
			throw new UnsupportedOperationException("not implemented");
		}
	}

}
