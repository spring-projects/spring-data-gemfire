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

import java.io.File;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.geode.cache.Region;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.util.FileCopyUtils;

/**
 * The SnapshotServiceImportExportIntegrationTest class is a test suite of test cases testing the import and export
 * of GemFire Cache Region data configured with SDG's Data Namespace &gt;gfe-data:snapshot-service&lt; (XML) element.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.support.ClassPathXmlApplicationContext
 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean
 * @see org.springframework.data.gemfire.repository.sample.Person
 * @see org.apache.geode.cache.Region
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class SnapshotServiceImportExportIntegrationTest {

	private static final AtomicLong ID_SEQUENCE = new AtomicLong(0L);

	private static ConfigurableApplicationContext applicationContext;

	private static File importPeopleSnapshot;
	private static File snapshotsDirectory;

	private static Region<Long, Person> people;

	protected static void assertPerson(Person expectedPerson, Person actualPerson) {

		assertThat(actualPerson, is(notNullValue()));
		assertThat(actualPerson.getId(), is(equalTo(expectedPerson.getId())));
		assertThat(actualPerson.getFirstname(), is(equalTo(expectedPerson.getFirstname())));
		assertThat(actualPerson.getLastname(), is(equalTo(expectedPerson.getLastname())));
	}

	protected static void assertRegion(Region<?, ?> actualRegion, String expectedName, int expectedSize) {

		assertThat(actualRegion, is(notNullValue()));
		assertThat(actualRegion.getName(), is(equalTo("People")));
		assertThat(actualRegion.getFullPath(), is(equalTo(String.format("%1$s%2$s", Region.SEPARATOR, expectedName))));
		assertThat(actualRegion.size(), is(expectedSize));
	}

	protected static Person newPerson(String firstName, String lastName) {
		return newPerson(ID_SEQUENCE.incrementAndGet(), firstName, lastName);
	}

	protected static Person newPerson(Long id, String firstName, String lastName) {
		return new Person(id, firstName, lastName);
	}

	@BeforeClass
	@SuppressWarnings("unchecked")
	public static void setupBeforeClass() throws Exception {

		snapshotsDirectory = new File(new File(FileSystemUtils.WORKING_DIRECTORY, "gemfire"), "snapshots");

		File exportDirectory = new File(snapshotsDirectory, "export");
		File importDirectory = new File(snapshotsDirectory, "import");

		assertThat(exportDirectory.isDirectory() || exportDirectory.mkdirs(), is(true));
		assertThat(importDirectory.isDirectory() || importDirectory.mkdirs(), is(true));

		importPeopleSnapshot = new File(importDirectory, "people-snapshot.gfd");

		FileCopyUtils.copy(new ClassPathResource("/people.snapshot").getFile(), importPeopleSnapshot);

		assertThat(importPeopleSnapshot.isFile(), is(true));
		assertThat(importPeopleSnapshot.length() > 0, is(true));

		applicationContext = new ClassPathXmlApplicationContext(
			SnapshotServiceImportExportIntegrationTest.class.getName().replace(".", "/").concat("-context.xml"));

		applicationContext.registerShutdownHook();

		people = applicationContext.getBean("People", Region.class);
	}

	@AfterClass
	public static void tearDownAfterClass() {

		applicationContext.close();

		File exportPeopleSnapshot = new File(new File(snapshotsDirectory, "export"), "people-snapshot.gfd");

		assertThat(exportPeopleSnapshot.isFile(), is(true));
		assertThat(exportPeopleSnapshot.length(), is(equalTo(importPeopleSnapshot.length())));

		FileSystemUtils.deleteRecursive(snapshotsDirectory.getParentFile());
	}

	@Before
	public void setup() {
		//setupPeople();
		ThreadUtils.timedWait(TimeUnit.SECONDS.toMillis(5), 500, () -> !(people.size() > 0));
	}

	protected void setupPeople() {
		put(newPerson("Jon", "Doe"));
		put(newPerson("Jane", "Doe"));
		put(newPerson("Cookie", "Doe"));
		put(newPerson("Fro", "Doe"));
		put(newPerson("Joe", "Doe"));
		put(newPerson("Lan", "Doe"));
		put(newPerson("Pie", "Doe"));
		put(newPerson("Play", "Doe"));
		put(newPerson("Sour", "Doe"));
	}

	protected Person put(Person person) {
		people.putIfAbsent(person.getId(), person);
		return person;
	}

	@Test
	public void peopleRegionIsLoaded() {
		assertRegion(people, "People", 9);
		assertPerson(people.get(1L), newPerson(1L, "Jon", "Doe"));
		assertPerson(people.get(2L), newPerson(2L, "Jane", "Doe"));
		assertPerson(people.get(3L), newPerson(3L, "Cookie", "Doe"));
		assertPerson(people.get(4L), newPerson(4L, "Fro", "Doe"));
		assertPerson(people.get(5L), newPerson(5L, "Joe", "Doe"));
		assertPerson(people.get(6L), newPerson(6L, "Lan", "Doe"));
		assertPerson(people.get(7L), newPerson(7L, "Pie", "Doe"));
		assertPerson(people.get(8L), newPerson(8L, "Play", "Doe"));
		assertPerson(people.get(9L), newPerson(9L, "Sour", "Doe"));
	}
}
