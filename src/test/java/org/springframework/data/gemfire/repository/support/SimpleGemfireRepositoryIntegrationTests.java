/*
 * Copyright 2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.repository.support;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import javax.annotation.Resource;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionEvent;
import org.apache.geode.cache.query.SelectResults;
import org.apache.geode.cache.util.CacheListenerAdapter;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.PeerCacheApplication;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.repository.core.EntityInformation;
import org.springframework.data.repository.core.support.ReflectionEntityInformation;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link SimpleGemfireRepository}.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.repository.support.SimpleGemfireRepository
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class SimpleGemfireRepositoryIntegrationTests {

	protected static final String DEFAULT_GEMFIRE_LOG_LEVEL = "warning";

	@Autowired
	private GemfireTemplate template;

	@Resource(name = "People")
	private Region<?, ?> people;

	private RegionClearListener regionClearListener;

	private SimpleGemfireRepository<Person, Long> repository;

	@Before
	@SuppressWarnings("unchecked")
	public void setUp() {
		people.clear();
		regionClearListener = new RegionClearListener();
		people.getAttributesMutator().addCacheListener(regionClearListener);
		EntityInformation<Person, Long> information = new ReflectionEntityInformation<>(Person.class);
		repository = new SimpleGemfireRepository<>(template, information);
	}

	@Test
	public void deleteAllFiresClearEvent() {
		assertThat(regionClearListener.eventFired).isFalse();
		repository.deleteAll();
		assertThat(regionClearListener.eventFired).isTrue();
	}

	@Test
	public void findAllWithIds() {
		Person dave = new Person(1L, "Dave", "Matthews");
		Person carter = new Person(2L, "Carter", "Beauford");
		Person leroi = new Person(3L, "Leroi", "Moore");

		template.put(dave.getId(), dave);
		template.put(carter.getId(), carter);
		template.put(leroi.getId(), leroi);

		Collection<Person> result = repository.findAll(Arrays.asList(carter.getId(), leroi.getId()));

		assertThat(result).isNotNull();
		assertThat(result.size()).isEqualTo(2);
		assertThat(result).containsAll(Arrays.asList(carter, leroi));
	}

	@Test
	public void findAllWithIdsReturnsNoMatches() {
		Collection<Person> results = repository.findAll(Arrays.asList(1L, 2L));

		assertThat(results).isNotNull();
		assertThat(results).isEmpty();
	}

	@Test
	public void findAllWithIdsReturnsPartialMatches() {
		Person kurt = new Person(1L, "Kurt", "Cobain");
		Person eddie = new Person(2L, "Eddie", "Veddar");
		Person michael = new Person(3L, "Michael", "Jackson");

		template.put(kurt.getId(), kurt);
		template.put(eddie.getId(), eddie);

		Collection<Person> results = repository.findAll(Arrays.asList(0L, 1L, 2L, 4L));

		assertThat(results).isNotNull();
		assertThat(results).hasSize(2);
		assertThat(results).contains(kurt, eddie);
		assertThat(results).doesNotContain(michael);
	}

	@Test
	public void queryRegion() throws Exception {
		Person oliverGierke = new Person(1L, "Oliver", "Gierke");

		assertThat(template.put(oliverGierke.getId(), oliverGierke)).isNull();

		SelectResults<Person> people = template.find("SELECT * FROM /People p WHERE p.firstname = $1",
			oliverGierke.getFirstname());

		assertThat(people.size()).isEqualTo(1);
		assertThat(people.iterator().next()).isEqualTo(oliverGierke);
	}

	@Test
	public void saveAndDeleteEntity() {
		Person oliverGierke = new Person(1L, "Oliver", "Gierke");

		assertThat(repository.save(oliverGierke)).isEqualTo(oliverGierke);
		assertThat(repository.count()).isEqualTo(1L);
		assertThat(repository.findOne(oliverGierke.getId()).orElse(null)).isEqualTo(oliverGierke);
		assertThat(repository.findAll()).isEqualTo(Collections.singletonList(oliverGierke));

		repository.delete(oliverGierke);

		assertThat(repository.count()).isEqualTo(0L);
		assertThat(repository.findOne(oliverGierke.getId()).orElse(null)).isNull();
		assertThat(repository.findAll()).isEmpty();
	}

	@Test
	public void saveEntities() {
		assertThat(template.getRegion()).isEmpty();

		Person johnBlum = new Person(1L, "John", "Blum");
		Person jonBloom = new Person(2L, "Jon", "Bloom");
		Person juanBlume = new Person(3L, "Juan", "Blume");

		repository.save(Arrays.asList(johnBlum, jonBloom, juanBlume));

		assertThat(template.getRegion().size()).isEqualTo(3);
		assertThat((Person) template.get(johnBlum.getId())).isEqualTo(johnBlum);
		assertThat((Person) template.get(jonBloom.getId())).isEqualTo(jonBloom);
		assertThat((Person) template.get(juanBlume.getId())).isEqualTo(juanBlume);
	}

	@SuppressWarnings("rawtypes")
	public static class RegionClearListener extends CacheListenerAdapter {

		public volatile boolean eventFired;

		@Override
		public void afterRegionClear(RegionEvent ev) {
			eventFired = true;
		}
	}

	@PeerCacheApplication(name = "SimpleGemfireRepositoryIntegrationTests", logLevel = DEFAULT_GEMFIRE_LOG_LEVEL)
	static class SimpleGemfireRepositoryConfiguration {

		@Bean(name = "People")
		LocalRegionFactoryBean<Object, Object> peopleRegion(GemFireCache gemfireCache) {
			LocalRegionFactoryBean<Object, Object> peopleRegion = new LocalRegionFactoryBean<>();

			peopleRegion.setCache(gemfireCache);
			peopleRegion.setClose(false);
			peopleRegion.setPersistent(false);

			return peopleRegion;
		}

		@Bean
		GemfireTemplate peopleRegionTemplate(Region<Object, Object> people) {
			return new GemfireTemplate(people);
		}
	}
}
