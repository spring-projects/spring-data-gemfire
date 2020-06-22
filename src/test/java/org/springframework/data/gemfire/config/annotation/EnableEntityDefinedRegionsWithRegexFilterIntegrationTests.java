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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientRegionShortcut;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.core.type.ClassMetadata;
import org.springframework.core.type.classreading.MetadataReader;
import org.springframework.core.type.filter.RegexPatternTypeFilter;
import org.springframework.data.gemfire.repository.sample.Account;
import org.springframework.data.gemfire.repository.sample.Programmer;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests for {@link EnableEntityDefinedRegions} and {@link EntityDefinedRegionsConfiguration}.
 *
 * @author John Blum
 * @see java.util.regex.Pattern
 * @see org.junit.Test
 * @see org.apache.geode.cache.Region
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions
 * @see org.springframework.data.gemfire.config.annotation.EntityDefinedRegionsConfiguration
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.4.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class EnableEntityDefinedRegionsWithRegexFilterIntegrationTests {

	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void programmerPatternMatchesLiteralProgrammer() {
		assertThat(Pattern.compile("Programmer").matcher("Programmer").find()).isTrue();
	}

	@Test
	public void regexPatternTypeFilterMatchesClass() throws IOException {

		ClassMetadata mockClassMetadata = mock(ClassMetadata.class);

		MetadataReader mockMetadataReader = mock(MetadataReader.class);

		doReturn(Programmer.class.getName()).when(mockClassMetadata).getClassName();
		doReturn(mockClassMetadata).when(mockMetadataReader).getClassMetadata();

		assertThat(new RegexPatternTypeFilter(Pattern.compile(".*Programmer"))
			.match(mockMetadataReader, null)).isTrue();
	}

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
		assertThat(regionBeanNames).hasSize(1);
		assertThat(regionBeanNames).containsExactly("/Programmers");
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableEntityDefinedRegions(
		basePackageClasses = Account.class,
		clientRegionShortcut = ClientRegionShortcut.LOCAL,
		includeFilters = @ComponentScan.Filter(type = FilterType.REGEX, pattern = ".*Programmer")
	)
	static class TestGeodeConfiguration { }

}
