/*
 * Copyright 2020 the original author or authors.
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
 */
package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientRegionShortcut;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.data.gemfire.repository.sample.Algorithm;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests for {@link EnableEntityDefinedRegions} and {@link EntityDefinedRegionsConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions
 * @see org.springframework.data.gemfire.config.annotation.EntityDefinedRegionsConfiguration
 * @since 2.4.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class EnableEntityDefinedRegionsWithAssignableTypeFilterIntegrationTests {

	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void onlyRegionsMatchedByFilterExist() {

		Set<String> regionBeanNames = Optional.ofNullable(this.applicationContext.getBeansOfType(Region.class))
			.map(Map::values)
			.orElseGet(Collections::emptySet)
			.stream()
			.filter(Objects::nonNull)
			.map(Region::getFullPath)
			.collect(Collectors.toSet());

		assertThat(regionBeanNames).isNotNull();
		assertThat(regionBeanNames).hasSize(4);
		assertThat(regionBeanNames)
			.containsExactlyInAnyOrder("/Users", "/Programmers", "/Local/Admin/Users", "/Local/Guest/Users");
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableEntityDefinedRegions(
		basePackageClasses = Algorithm.class,
		clientRegionShortcut = ClientRegionShortcut.LOCAL,
		excludeFilters = @ComponentScan.Filter(type = FilterType.REGEX, pattern = "Programmer"),
		includeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = User.class)
	)
	static class TestGeodeConfiguration { }

}
