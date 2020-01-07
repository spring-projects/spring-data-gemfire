/*
 * Copyright 2016-2020 the original author or authors.
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
package org.springframework.data.gemfire.support;

import java.util.Properties;

import org.apache.geode.cache.Cache;
import org.apache.geode.distributed.ServerLauncher;
import org.apache.geode.distributed.ServerLauncher.Builder;
import org.apache.geode.distributed.ServerLauncherCacheProvider;

import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.GemfireUtils;

/**
 * The {@link SpringServerLauncherCacheProvider} class overrides the default behavior of Pivotal GemFire's
 * {@link ServerLauncher} to bootstrap the Pivotal GemFire cache using a Spring {@link ApplicationContext}
 * instead of Pivotal GemFire {@literal cache.xml} inside a Pivotal GemFire Server JVM-based process.
 * This enables a Pivotal GemFire Cache Server's resources to be configured with SDG's XML namespace.
 *
 * Unlike {@link SpringContextBootstrappingInitializer}, this allows the configuration of the cache to specified
 * in the Spring context.
 *
 * To use this cache provider, ensure that the SDG JAR file is on the classpath of the Pivotal GemFire server
 * and specify the --spring-xml-location option from the Gfsh command line or call
 * {@link Builder#setSpringXmlLocation(String)} when launching the Pivotal GemFire server.
 *
 * @author Dan Smith
 * @author John Blum
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
 * @see org.apache.geode.distributed.ServerLauncherCacheProvider
 * @since 1.7.0
 * @link https://gemfire.docs.pivotal.io/latest/userguide/index.html#basic_config/the_cache/setting_cache_initializer.html
 */
public class SpringServerLauncherCacheProvider implements ServerLauncherCacheProvider {

	@Override
	public Cache createCache(Properties gemfireProperties, ServerLauncher serverLauncher) {
		Cache cache = null;

		if (serverLauncher.isSpringXmlLocationSpecified()) {
			System.setProperty(gemfireName(), serverLauncher.getMemberName());
			newSpringContextBootstrappingInitializer().init(createParameters(serverLauncher));
			cache = SpringContextBootstrappingInitializer.getApplicationContext().getBean(Cache.class);
		}

		return cache;
	}

	Properties createParameters(ServerLauncher serverLauncher) {
		Properties parameters = new Properties();
		parameters.setProperty(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
			serverLauncher.getSpringXmlLocation());
		return parameters;
	}

	String gemfireName() {
		return (GemfireUtils.GEMFIRE_PREFIX + GemfireUtils.NAME_PROPERTY_NAME);
	}

	SpringContextBootstrappingInitializer newSpringContextBootstrappingInitializer() {
		return new SpringContextBootstrappingInitializer();
	}
}
