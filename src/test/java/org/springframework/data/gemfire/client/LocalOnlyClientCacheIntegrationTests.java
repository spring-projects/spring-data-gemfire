/*
 * Copyright 2012-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.client;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Calendar;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.config.annotation.ClientCacheApplication;
import org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions;
import org.springframework.data.gemfire.test.model.Gender;
import org.springframework.data.gemfire.test.model.Person;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests testing the configuration of a {@link ClientRegionShortcut#LOCAL}
 * {@link ClientCache} {@link Region}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @since 1.6.3
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class LocalOnlyClientCacheIntegrationTests {

	@Resource(name = "People")
	private Region<Long, Person> people;

	@Before
	public void assertPeopleRegionConfiguration() {

		assertThat(this.people).isNotNull();
		assertThat(this.people.getName()).isEqualTo("People");
		assertThat(this.people.getFullPath()).isEqualTo(GemfireUtils.toRegionPath("People"));
		assertThat(this.people.getAttributes()).isNotNull();
		assertThat(this.people.getAttributes().getDataPolicy()).isEqualTo(DataPolicy.NORMAL);
		assertThat(this.people.getAttributes().getKeyConstraint()).isEqualTo(Long.class);
		assertThat(this.people.getAttributes().getValueConstraint()).isEqualTo(Person.class);
		assertThat(this.people).hasSize(0);
	}

	@Test
	public void putAndGetPersonIsSuccessful() {

		Person jonDoe = Person.newPerson("Jon", "Doe",
			Person.newBirthDate(1974, Calendar.MAY, 5), Gender.MALE);

		assertThat(this.people).hasSize(0);
		assertThat(this.people.put(jonDoe.getId(), jonDoe));
		assertThat(this.people).hasSize(1);
		assertThat(this.people.get(jonDoe.getId())).isEqualTo(jonDoe);
		assertThat(this.people).hasSize(1);
	}

	@ClientCacheApplication
	@EnableEntityDefinedRegions(basePackageClasses = Person.class,
		clientRegionShortcut = ClientRegionShortcut.LOCAL, strict = true)
	static class GemFireClientCacheConfiguration {
	}
}
