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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.data.gemfire.test.support.IdentifierSequence;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.QueryService;

/**
 * The CreateDefinedIndexesIntegrationTest class is a test suite of test cases testing the functional behavior
 * of the GemFire Cache Region Index "definition" and subsequent "creation" step in a SDG-configured application
 * using a combination of "defined" and "created" Index beans in a Spring context.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.query.Index
 * @see com.gemstone.gemfire.cache.query.QueryService
 * @since 1.7.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class CreateDefinedIndexesIntegrationTest {

	private static final List<String> definedIndexNames = new ArrayList<String>(3);

	@Autowired
	private Cache gemfireCache;

	@Autowired
	@Qualifier("IdIdx")
	private Index id;

	@Autowired
	@Qualifier("BirthDateIdx")
	private Index birthDate;

	@Autowired
	@Qualifier("FullNameIdx")
	private Index fullName;

	@Autowired
	@Qualifier("LastNameIdx")
	private Index lastName;

	@Resource(name = "People")
	private Region<Long, Person> people;

	protected static Date createBirthDate(final int year, final int month, final int dayOfMonth) {
		Calendar birthDate = Calendar.getInstance();
		birthDate.clear();
		birthDate.set(Calendar.YEAR, year);
		birthDate.set(Calendar.MONTH, month);
		birthDate.set(Calendar.DAY_OF_MONTH, dayOfMonth);
		return birthDate.getTime();
	}

	protected static Person createPerson(final String firstName, final String lastName, final Date birthDate) {
		return createPerson(IdentifierSequence.nextId(), firstName, lastName, birthDate);
	}

	protected static Person createPerson(final Long id, final String firstName, final String lastName, final Date birthDate) {
		return new Person(id, firstName, lastName, birthDate);
	}

	protected static Person put(final Region<Long, Person> people, final Person person) {
		people.put(person.getId(), person);
		return person;
	}

	@Before
	public void setup() {
		put(people, createPerson("Jon", "Doe", createBirthDate(1989, Calendar.NOVEMBER, 11)));
		put(people, createPerson("Jane", "Doe", createBirthDate(1991, Calendar.APRIL, 4)));
		put(people, createPerson("Pie", "Doe", createBirthDate(2008, Calendar.JUNE, 21)));
		put(people, createPerson("Cookie", "Doe", createBirthDate(2008, Calendar.AUGUST, 14)));
	}

	@Test
	public void indexesCreated() {
		QueryService queryService = gemfireCache.getQueryService();

		//System.out.printf("GemFire Cache Indexes: %1$s%n", queryService.getIndexes());
		//System.out.printf("/Example Region Indexes: %1$s%n", queryService.getIndexes(people));

		assertTrue(definedIndexNames.containsAll(Arrays.asList(id.getName(), birthDate.getName(), fullName.getName())));
		assertEquals(id, queryService.getIndex(people, id.getName()));
		assertEquals(birthDate, queryService.getIndex(people, birthDate.getName()));
		assertEquals(fullName, queryService.getIndex(people, fullName.getName()));
		assertEquals(lastName, queryService.getIndex(people, lastName.getName()));
	}

	public static class IndexBeanPostProcessor implements BeanPostProcessor {

		@Override
		public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
			return bean;
		}

		@Override
		public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
			if (bean instanceof Index) {
				if ("LastNameIdx".equals(beanName)) {
					assertTrue(CacheFactory.getAnyInstance().getQueryService().getIndexes().contains(bean));
					assertTrue(beanName.equalsIgnoreCase(((Index) bean).getName()));
				}
				else {
					definedIndexNames.add(beanName);
				}
			}

			return bean;
		}
	}

	public static class Person implements Serializable {

		protected static final String BIRTH_DATE_FORMAT_PATTERN = "yyyy/MM/dd";

		private Date birthDate;

		private Long id;

		private String firstName;
		private String lastName;

		public Person() {
		}

		public Person(final Long id) {
			this.id = id;
		}

		public Person(final String firstName, final String lastName) {
			this.firstName = firstName;
			this.lastName = lastName;
		}

		public Person(final String firstName, final String lastName, final Date birthDate) {
			this(firstName, lastName);
			this.birthDate = (birthDate != null ? (Date) birthDate.clone() : null);
		}

		public Person(final Long id, final String firstName, final String lastName, final Date birthDate) {
			this(firstName, lastName, birthDate);
			this.id = id;
		}

		public Long getId() {
			return id;
		}

		public Date getBirthDate() {
			return birthDate;
		}

		public String getFirstName() {
			return firstName;
		}

		public String getFullName() {
			return String.format("%1$s %2$s", getFirstName(), getLastName());
		}

		public String getLastName() {
			return lastName;
		}

		protected static boolean equalsIgnoreNull(final Object obj1, final Object obj2) {
			return (obj1 == null ? obj2 == null : obj1.equals(obj2));
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == this) {
				return true;
			}

			if (!(obj instanceof Person)) {
				return false;
			}

			Person that = (Person) obj;

			return equalsIgnoreNull(getId(), that.getId())
				&& (ObjectUtils.nullSafeEquals(getBirthDate(), that.getBirthDate()))
				&& (ObjectUtils.nullSafeEquals(getFirstName(), that.getFirstName())
				&& (ObjectUtils.nullSafeEquals(getLastName(), that.getLastName())));
		}

		@Override
		public int hashCode() {
			int hashValue = 17;
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getId());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getBirthDate());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getFirstName());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getLastName());
			return hashValue;
		}

		protected static String toString(final Date dateTime, final String DATE_FORMAT_PATTERN) {
			return (dateTime == null ? null : new SimpleDateFormat(DATE_FORMAT_PATTERN).format(dateTime));
		}

		@Override
		public String toString() {
			return String.format("{ @type = %1$s, id = %2$d, firstName = %3$s, lastName = %4$s, birthDate = %5$s }",
				getClass().getName(), getId(), getFirstName(), getLastName(),
					toString(getBirthDate(), BIRTH_DATE_FORMAT_PATTERN));
		}
	}

}
