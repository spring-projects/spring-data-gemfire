/*
 * Copyright 2016-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration.DEFAULT_MCAST_PORT;

import java.util.Properties;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link AbstractCacheConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration
 * @since 1.9.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractCacheConfigurationUnitTests {

	@Spy
	private AbstractCacheConfiguration cacheConfiguration;

	@Test
	public void gemfirePropertiesContainsEssentialProperties() {

		this.cacheConfiguration.setName("TestName");
		this.cacheConfiguration.setMcastPort(-1);
		this.cacheConfiguration.setLogLevel("DEBUG");
		this.cacheConfiguration.setLocators("skullbox[11235]");
		this.cacheConfiguration.setStartLocator("boombox[12480]");

		Properties gemfireProperties = this.cacheConfiguration.gemfireProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties).hasSize(5);
		assertThat(gemfireProperties.getProperty("name")).isEqualTo("TestName");
		assertThat(gemfireProperties.getProperty("mcast-port")).isEqualTo(String.valueOf(DEFAULT_MCAST_PORT));
		assertThat(gemfireProperties.getProperty("log-level")).isEqualTo("DEBUG");
		assertThat(gemfireProperties.getProperty("locators")).isEqualTo("skullbox[11235]");
		assertThat(gemfireProperties.getProperty("start-locator")).isEqualTo("boombox[12480]");
	}
}
