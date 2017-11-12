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

package org.springframework.data.gemfire.snapshot;

import static java.util.Arrays.stream;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.io.File;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.snapshot.SnapshotFilter;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.snapshot.event.ExportSnapshotApplicationEvent;
import org.springframework.data.gemfire.snapshot.event.ImportSnapshotApplicationEvent;
import org.springframework.data.gemfire.snapshot.event.SnapshotApplicationEvent;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The SnapshotApplicationEventTriggeredImportsExportsIntegrationTest class is a test suite of test cases testing
 * the effects of the SnapshotServiceFactoryBean using Spring ApplicationEvents to trigger imports and exports
 * of GemFire Cache Region data.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.springframework.data.gemfire.snapshot.event.ExportSnapshotApplicationEvent
 * @see org.springframework.data.gemfire.snapshot.event.ImportSnapshotApplicationEvent
 * @see org.springframework.data.gemfire.snapshot.event.SnapshotApplicationEvent
 * @see org.springframework.data.gemfire.snapshot.SnapshotServiceFactoryBean
 * @see org.springframework.data.gemfire.repository.sample.Person
 * @see org.apache.geode.cache.Region
 * @since 1.7.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class SnapshotApplicationEventTriggeredImportsExportsIntegrationTest {

	protected static final AtomicLong ID_SEQUENCE = new AtomicLong(0L);

	protected static File snapshotsDirectory;

	@Autowired
	private ApplicationEventPublisher eventPublisher;

	@Resource(name = "Doe")
	private Region<Long, Person> doe;

	@Resource(name = "EveryoneElse")
	private Region<Long, Person> everyoneElse;

	@Resource(name = "Handy")
	private Region<Long, Person> handy;

	@Resource(name = "People")
	private Region<Long, Person> people;

	@BeforeClass
	public static void setupBeforeClass() throws Exception {

		snapshotsDirectory = new File(new File(FileSystemUtils.WORKING_DIRECTORY, "gemfire"), "snapshots");

		assertThat(snapshotsDirectory.isDirectory() || snapshotsDirectory.mkdirs(), is(true));

		File peopleSnapshotFile = new File(snapshotsDirectory, "people-snapshot.gfd");
		File nonHandyNonDoeSnapshotFile = new File(snapshotsDirectory, "nonHandyNonDoePeople-snapshot.gfd");

		assertThat(peopleSnapshotFile.isFile() || peopleSnapshotFile.createNewFile(), is(true));
		assertThat(nonHandyNonDoeSnapshotFile.isFile() || nonHandyNonDoeSnapshotFile.createNewFile(), is(true));
	}

	@AfterClass
	public static void tearDownAfterClass() {
		//FileSystemUtils.deleteRecursive(snapshotsDirectory.getParentFile());
	}

	protected void assertPeople(Region<Long, Person> targetRegion, Person... people) {

		assertThat(targetRegion.size(), is(equalTo(people.length)));

		stream(nullSafeArray(people, Person.class))
			.forEach(person -> assertPerson(person, targetRegion.get(person.getId())));
	}

	protected void assertPerson(Person expectedPerson, Person actualPerson) {

		assertThat(String.format("Expected [%1$s]; but was [%2$s]", expectedPerson, actualPerson),
			actualPerson, is(notNullValue()));
		assertThat(actualPerson.getId(), is(equalTo(expectedPerson.getId())));
		assertThat(actualPerson.getFirstname(), is(equalTo(expectedPerson.getFirstname())));
		assertThat(actualPerson.getLastname(), is(equalTo(expectedPerson.getLastname())));
	}

	protected Person newPerson(String firstName, String lastName) {
		return new Person(ID_SEQUENCE.incrementAndGet(), firstName, lastName);
	}

	protected Person put(Region<Long, Person> targetRegion, Person person) {

		targetRegion.putIfAbsent(person.getId(), person);

		return person;
	}

	protected void wait(int seconds, int expectedDoeSize, int expectedEveryoneSize, int expectedHandySize) {

		ThreadUtils.timedWait(TimeUnit.SECONDS.toMillis(seconds), 500,
			() -> (doe.size() < expectedDoeSize && everyoneElse.size() <
				expectedEveryoneSize && handy.size() < expectedHandySize));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void exportsTriggeringImportsOnSnapshotApplicationEvents() {

		Person jonDoe = put(people, newPerson("Jon", "Doe"));
		Person janeDoe = put(people, newPerson("Jane", "Doe"));
		Person jackBlack = put(people, newPerson("Jack", "Black"));
		Person jackHandy = put(people, newPerson("Jack", "Handy"));
		Person joeDirt = put(people, newPerson("Joe", "Dirt"));

		SnapshotApplicationEvent event =
			new ExportSnapshotApplicationEvent<Long, Person>(this, people.getFullPath());

		eventPublisher.publishEvent(event);

		wait(5, 2, 2, 1);

		assertPeople(doe, jonDoe, janeDoe);
		assertPeople(everyoneElse, jackBlack, joeDirt);
		assertPeople(handy, jackHandy);

		Person cookieDoe = put(people, newPerson("Cookie", "Doe"));
		Person pieDoe = put(people, newPerson("Pie", "Doe"));
		Person sourDoe = put(people, newPerson("Sour", "Doe"));
		Person randyHandy = put(people, newPerson("Randy", "Handy"));
		Person sandyHandy = put(people, newPerson("Sandy", "Handy"));
		Person jackHill = put(people, newPerson("Jack", "Hill"));
		Person jillHill = put(people, newPerson("Jill", "Hill"));

		eventPublisher.publishEvent(event);

		wait(10, 5, 4, 3);

		assertPeople(doe, jonDoe, janeDoe, cookieDoe, pieDoe, sourDoe);
		assertPeople(everyoneElse, jackBlack, joeDirt, jackHill, jillHill);
		assertPeople(handy, jackHandy, randyHandy, sandyHandy);

		Person bobDoe = put(people, newPerson("Bob", "Doe"));
		Person mandyHandy = put(people, newPerson("Mandy", "Handy"));
		Person imaPigg = put(people, newPerson("Ima", "Pigg"));
		Person benDover = put(people, newPerson("Ben", "Dover"));

		eventPublisher.publishEvent(event);

		wait(15, 6, 6, 4);

		assertPeople(doe, jonDoe, janeDoe, cookieDoe, pieDoe, sourDoe, bobDoe);
		assertPeople(everyoneElse, jackBlack, joeDirt, jackHill, jillHill, imaPigg, benDover);
		assertPeople(handy, jackHandy, randyHandy, sandyHandy, mandyHandy);
	}

	protected static class LastNameSnapshotFilter implements SnapshotFilter<Long, Person> {

		private final String lastName;

		public LastNameSnapshotFilter(String lastName) {
			Assert.hasText(lastName, "'lastName' must be specified");
			this.lastName = lastName;
		}

		protected String getLastName() {
			Assert.state(StringUtils.hasText(lastName), "'lastName' was not properly initialized");
			return lastName;
		}

		@Override
		public boolean accept(Map.Entry<Long, Person> entry) {
			return accept(entry.getValue());
		}

		public boolean accept(Person person) {
			return getLastName().equalsIgnoreCase(person.getLastname());
		}
	}

	protected static class NotLastNameSnapshotFilter extends LastNameSnapshotFilter {

		public NotLastNameSnapshotFilter(String lastName) {
			super(lastName);
		}

		@Override
		public boolean accept(final Map.Entry<Long, Person> entry) {
			return !super.accept(entry);
		}
	}

	protected static class SnapshotImportsMonitor {

		@Autowired
		private ApplicationEventPublisher eventPublisher;

		private static final Map<File, Long> snapshotFileLastModifiedMap = new ConcurrentHashMap<File, Long>(2);

		@Scheduled(fixedDelay = 1000)
		@SuppressWarnings("unchecked")
		public void processSnapshots() {

			boolean triggerEvent = false;

			for (File snapshotFile : nullSafeArray(snapshotsDirectory.listFiles(FileSystemUtils.FileOnlyFilter.INSTANCE))) {
				triggerEvent |= isUnprocessedSnapshotFile(snapshotFile);
			}

			if (triggerEvent) {
				eventPublisher.publishEvent(new ImportSnapshotApplicationEvent<Long, Person>(this));
			}
		}

		protected File[] nullSafeArray(File... files) {
			return (files != null ? files : new File[0]);
		}

		protected boolean isUnprocessedSnapshotFile(File snapshotFile) {

			Long lastModified = snapshotFile.lastModified();
			Long previousLastModified = snapshotFileLastModifiedMap.get(snapshotFile);

			previousLastModified = (previousLastModified != null ? previousLastModified : lastModified);

			snapshotFileLastModifiedMap.put(snapshotFile, lastModified);

			return !previousLastModified.equals(lastModified);
		}
	}
}
