/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.cache;

import static org.assertj.core.api.Assertions.*;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import java.io.Serializable;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.annotation.Resource;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import org.apache.geode.cache.GemFireCache;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.Caching;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.gemfire.test.support.IdentifierSequence;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Service;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Integration tests testing the contractual behavior and combination of using Spring'a {@link CachePut} annotation
 * followed by a {@link CacheEvict} annotation on an application {@link @Service} component.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.cache.annotation.CacheEvict
 * @see org.springframework.cache.annotation.CachePut
 * @see org.springframework.cache.annotation.Caching
 * @see org.springframework.cache.annotation.EnableCaching
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see GemfireCache#evict(Object)
 * @see GemfireCache#put(Object, Object)
 * @see <a href="https://stackoverflow.com/questions/39830488/gemfire-entrynotfoundexception-for-cacheevict">Gemfire EntryNotFoundException on @CacheEvict</a>
 * @see <a href="https://jira.spring.io/browse/SGF-539">Change GemfireCache.evict(key) to call Region.remove(key)</a>
 * @since 1.9.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CompoundCachePutCacheEvictIntegrationTests.ApplicationTestConfiguration.class)
@SuppressWarnings("unused")
public class CompoundCachePutCacheEvictIntegrationTests {

	private Person janeDoe;
	private Person jonDoe;

	@Autowired
	private PeopleService peopleService;

	@Resource(name = "People")
	private org.apache.geode.cache.Region<Long, Person> peopleRegion;

	protected void assertNoPeopleInDepartment(Department department) {
		assertPeopleInDepartment(department);
	}

	protected void assertPeopleInDepartment(Department department, Person... people) {
		List<Person> peopleInDepartment = peopleService.findByDepartment(department);

		assertThat(peopleInDepartment).isNotNull();
		assertThat(peopleInDepartment.size()).isEqualTo(people.length);
		assertThat(peopleInDepartment).contains(people);
	}

	protected Person newPerson(String name, String mobile, Department department) {
		return newPerson(IdentifierSequence.nextId(), name, mobile, department);
	}

	protected Person newPerson(Long id, String name, String mobile, Department department) {
		Person person = Person.newPerson(department, mobile, name);
		person.setId(id);
		return person;
	}

	protected Person save(Person person) {
		peopleRegion.put(person.getId(), person);
		return person;
	}

	@Before
	public void setup() {
		janeDoe = save(newPerson("Jane Doe", "541-555-1234", Department.MARKETING));
		jonDoe = save(newPerson("Jon Doe", "972-555-1248", Department.ENGINEERING));

		assertThat(peopleRegion.containsValue(janeDoe)).isTrue();
		assertThat(peopleRegion.containsValue(janeDoe)).isTrue();
	}

	@Test
	public void janeDoeUpdateSuccessful() {
		assertNoPeopleInDepartment(Department.DESIGN);
		assertThat(peopleService.isCacheMiss()).isTrue();

		janeDoe.setDepartment(Department.DESIGN);
		peopleService.update(janeDoe);

		assertPeopleInDepartment(Department.DESIGN, janeDoe);
		assertThat(peopleService.isCacheMiss()).isTrue();
	}

	@Test
	public void jonDoeUpdateSuccessful() {
		jonDoe.setDepartment(Department.RESEARCH_DEVELOPMENT);
		peopleService.update(jonDoe);

		assertPeopleInDepartment(Department.RESEARCH_DEVELOPMENT, jonDoe);
		assertThat(peopleService.isCacheMiss()).isTrue();
	}

	@Configuration
	@EnableCaching
	@Import(ApplicationTestConfiguration.class)
	static class Sgf539WorkaroundConfiguration {

		@Bean
		GemfireCacheManager cacheManager(GemFireCache gemfireCache) {
			GemfireCacheManager cacheManager = new GemfireCacheManager() {
				@Override protected org.springframework.cache.Cache decorateCache(org.springframework.cache.Cache cache) {
					return new GemfireCache((org.apache.geode.cache.Region<?, ?>) cache.getNativeCache()) {
						@Override public void evict(Object key) {
							getNativeCache().remove(key);
						}
					};
				}
			};

			cacheManager.setCache(gemfireCache);

			return cacheManager;
		}
	}

	@Configuration
	@EnableCaching
	@Import(GemFireConfiguration.class)
	static class ApplicationTestConfiguration {

		@Bean
		GemfireCacheManager cacheManager(GemFireCache gemfireCache) {
			GemfireCacheManager cacheManager = new GemfireCacheManager();
			cacheManager.setCache(gemfireCache);
			return cacheManager;
		}

		@Bean
		GemfireRepositoryFactoryBean<PersonRepository, Person, Long> personRepository() {
			GemfireRepositoryFactoryBean<PersonRepository, Person, Long> personRepository =
				new GemfireRepositoryFactoryBean<PersonRepository, Person, Long>(PersonRepository.class);

			personRepository.setGemfireMappingContext(new GemfireMappingContext());

			return personRepository;
		}

		@Bean
		PeopleService peopleService(PersonRepository personRepository) {
			return new PeopleService(personRepository);
		}
	}

	@Configuration
	static class GemFireConfiguration {

		static final String DEFAULT_GEMFIRE_LOG_LEVEL = "error";

		Properties gemfireProperties() {

			Properties gemfireProperties = new Properties();

			gemfireProperties.setProperty("name", applicationName());
			gemfireProperties.setProperty("locators", "");
			gemfireProperties.setProperty("log-level", logLevel());

			return gemfireProperties;
		}

		String applicationName() {
			return CompoundCachePutCacheEvictIntegrationTests.class.getName();
		}

		String logLevel() {
			return System.getProperty("spring.data.gemfire.log.level", DEFAULT_GEMFIRE_LOG_LEVEL);
		}

		@Bean
		CacheFactoryBean gemfireCache() {
			CacheFactoryBean gemfireCache = new CacheFactoryBean();

			gemfireCache.setClose(true);
			gemfireCache.setProperties(gemfireProperties());

			return gemfireCache;
		}

		@Bean(name = "People")
		LocalRegionFactoryBean<Long, Person> peopleRegion(GemFireCache gemfireCache) {
			LocalRegionFactoryBean<Long, Person> peopleRegion = new LocalRegionFactoryBean<Long, Person>();

			peopleRegion.setCache(gemfireCache);
			peopleRegion.setClose(false);
			peopleRegion.setPersistent(false);

			return peopleRegion;
		}

		@Bean(name = "DepartmentPeople")
		LocalRegionFactoryBean<Long, Person> departmentPeopleRegion(GemFireCache gemfireCache) {
			LocalRegionFactoryBean<Long, Person> departmentPeopleRegion = new LocalRegionFactoryBean<Long, Person>();

			departmentPeopleRegion.setCache(gemfireCache);
			departmentPeopleRegion.setClose(false);
			departmentPeopleRegion.setPersistent(false);

			return departmentPeopleRegion;
		}

		@Bean(name = "MobilePeople")
		LocalRegionFactoryBean<Long, Person> mobilePeopleRegion(GemFireCache gemfireCache) {
			LocalRegionFactoryBean<Long, Person> mobilePeopleRegion = new LocalRegionFactoryBean<Long, Person>();

			mobilePeopleRegion.setCache(gemfireCache);
			mobilePeopleRegion.setClose(false);
			mobilePeopleRegion.setPersistent(false);

			return mobilePeopleRegion;
		}
	}

	public enum Department {
		ACCOUNTING,
		DESIGN,
		ENGINEERING,
		LEGAL,
		MANAGEMENT,
		MARKETING,
		RESEARCH_DEVELOPMENT,
		SALES
	}

	@Data
	@Region("People")
	@RequiredArgsConstructor(staticName = "newPerson")
	public static class Person implements Serializable {

		@Id
		private Long id;

		@NonNull private Department department;
		@NonNull private String mobile;
		@NonNull private String name;

	}

	@Service
	public static class PeopleService extends CacheableService {

		private final PersonRepository personRepository;

		public PeopleService(PersonRepository personRepository) {
			this.personRepository = personRepository;
		}

		@Cacheable("DepartmentPeople")
		public List<Person> findByDepartment(Department department) {
			setCacheMiss();
			return personRepository.findByDepartment(department);
		}

		@Cacheable("MobilePeople")
		public Person findByMobile(String mobile) {
			setCacheMiss();
			return personRepository.findByMobile(mobile);
		}

		@Caching(
			evict = @CacheEvict(value = "DepartmentPeople", key = "#p0.department"),
			put = @CachePut(value = "MobilePeople", key="#p0.mobile")
		)
		public Person update(Person person) {
			return personRepository.save(person);
		}
	}

	protected static abstract class CacheableService {

		private final AtomicBoolean cacheMiss = new AtomicBoolean(false);

		public boolean isCacheMiss() {
			return cacheMiss.compareAndSet(true, false);
		}

		public boolean isNotCacheMiss() {
			return !isCacheMiss();
		}

		protected void setCacheMiss() {
			this.cacheMiss.set(true);
		}
	}
	public interface PersonRepository extends CrudRepository<Person, Long> {

		List<Person> findByDepartment(Department department);

		Person findByMobile(String mobile);

	}
}
