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

package org.springframework.data.gemfire.config.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.data.gemfire.test.model.Person.newBirthDate;
import static org.springframework.data.gemfire.test.model.Person.newPerson;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;

import javax.annotation.Resource;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.QueryService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.DependsOn;
import org.springframework.data.gemfire.IndexFactoryBean;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.PeerCacheApplication;
import org.springframework.data.gemfire.test.model.Gender;
import org.springframework.data.gemfire.test.model.Person;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Integration tests for {@link DefinedIndexesApplicationListener}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.query.Index
 * @see org.apache.geode.cache.query.QueryService
 * @since 1.7.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class DefinedIndexesIntegrationTests {

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
	@Qualifier("LastNameIdx")
	private Index lastName;

	@Autowired
	@Qualifier("NameIdx")
	private Index name;

	@Resource(name = "People")
	private Region<Long, Person> people;

	protected static Person put(Region<Long, Person> people, Person person) {
		people.put(person.getId(), person);
		return person;
	}

	@Before
	public void setup() {
		put(people, newPerson("Jon", "Doe", newBirthDate(1989, Calendar.NOVEMBER, 11), Gender.MALE));
		put(people, newPerson("Jane", "Doe", newBirthDate(1991, Calendar.APRIL, 4), Gender.FEMALE));
		put(people, newPerson("Pie", "Doe", newBirthDate(2008, Calendar.JUNE, 21), Gender.FEMALE));
		put(people, newPerson("Cookie", "Doe", newBirthDate(2008, Calendar.AUGUST, 14), Gender.FEMALE));
	}

	@Test
	public void indexesCreated() {
		QueryService queryService = gemfireCache.getQueryService();

		List<String> expectedDefinedIndexNames = Arrays.asList(id.getName(), birthDate.getName(), name.getName());

		assertThat(definedIndexNames).isEqualTo(expectedDefinedIndexNames);
		assertThat(id).isEqualTo(queryService.getIndex(people, id.getName()));
		assertThat(birthDate).isEqualTo(queryService.getIndex(people, birthDate.getName()));
		assertThat(lastName).isEqualTo(queryService.getIndex(people, lastName.getName()));
		assertThat(name).isEqualTo(queryService.getIndex(people, name.getName()));
	}

	@PeerCacheApplication(logLevel = "warning")
	static class DefinedIndexesConfiguration {

		@Bean
		// TODO remove when the Annotation config model includes support
		DefinedIndexesApplicationListener indexApplicationListener() {
			return new DefinedIndexesApplicationListener();
		}

		@Bean
		BeanPostProcessor indexBeanPostProcessor() {
			return new BeanPostProcessor() {

				@Override
				public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
					return bean;
				}

				@Override
				public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
					if (bean instanceof Index) {
						if ("LastNameIdx".equals(beanName)) {
							assertThat(CacheFactory.getAnyInstance().getQueryService().getIndexes().contains(bean)).isTrue();
							assertThat(beanName).isEqualToIgnoringCase(((Index) bean).getName());
						}
						else {
							definedIndexNames.add(beanName);
						}
					}

					return bean;
				}
			};
		}

		@Bean(name = "People")
		PartitionedRegionFactoryBean<Long, Person> peopleRegion(Cache gemfireCache) {
			PartitionedRegionFactoryBean<Long, Person> peopleRegion = new PartitionedRegionFactoryBean<Long, Person>();

			peopleRegion.setCache(gemfireCache);
			peopleRegion.setClose(false);
			peopleRegion.setPersistent(false);

			return peopleRegion;
		}

		@Bean(name = "IdIdx")
		@DependsOn("People")
		IndexFactoryBean idIndex(GemFireCache gemFireCache) {
			IndexFactoryBean idIndex = new IndexFactoryBean();

			idIndex.setCache(gemFireCache);
			idIndex.setDefine(true);
			idIndex.setExpression("id");
			idIndex.setFrom("/People");
			idIndex.setName("IdIdx");
			idIndex.setType(IndexType.KEY);

			return idIndex;
		}

		@Bean(name = "BirthDateIdx")
		@DependsOn("People")
		IndexFactoryBean birthDateIndex(GemFireCache gemFireCache) {
			IndexFactoryBean birthDateIndex = new IndexFactoryBean();

			birthDateIndex.setCache(gemFireCache);
			birthDateIndex.setDefine(true);
			birthDateIndex.setExpression("birthDate");
			birthDateIndex.setFrom("/People");
			birthDateIndex.setName("BirthDateIdx");
			birthDateIndex.setType(IndexType.HASH);

			return birthDateIndex;
		}

		@Bean(name = "LastNameIdx")
		@DependsOn("People")
		IndexFactoryBean lastNameIndex(GemFireCache gemFireCache) {
			IndexFactoryBean lastNameIndex = new IndexFactoryBean();

			lastNameIndex.setCache(gemFireCache);
			lastNameIndex.setExpression("lastName");
			lastNameIndex.setFrom("/People");
			lastNameIndex.setName("LastNameIdx");
			lastNameIndex.setType(IndexType.HASH);

			return lastNameIndex;
		}

		@Bean(name = "NameIdx")
		@DependsOn("People")
		IndexFactoryBean nameIndex(GemFireCache gemFireCache) {
			IndexFactoryBean nameIndex = new IndexFactoryBean();

			nameIndex.setCache(gemFireCache);
			nameIndex.setDefine(true);
			nameIndex.setExpression("name");
			nameIndex.setFrom("/People");
			nameIndex.setName("NameIdx");
			nameIndex.setType(IndexType.FUNCTIONAL);

			return nameIndex;
		}
	}
}
