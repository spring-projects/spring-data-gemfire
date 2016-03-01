/*
 * Copyright 2016 the original author or authors.
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
package org.springframework.data.gemfire.support;

import java.util.Collections;
import java.util.Properties;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.distributed.ServerLauncher;
import com.gemstone.gemfire.distributed.ServerLauncher.Builder;
import com.gemstone.gemfire.distributed.internal.DistributionConfig;
import com.gemstone.gemfire.internal.util.CollectionUtils;
import com.gemstone.gemfire.distributed.ServerLauncherCacheProvider;

/**
 * The SpringServerLauncherCacheProvider class is overrides the default behavior
 * of GemFire's ServerLauncher to bootstrap the cache using a Spring
 * ApplicationContext instead of a GemFire cache.xml inside a GemFire Server
 * JVM-based process. This enables a GemFire Cache Server resources to be
 * configured with Spring Data GemFire's XML namespace.
 * 
 * Unlike {@link SpringContextBootstrappingInitializer}, this allows the configuration
 * of the cache to specified in the Spring Context.
 * 
 * To use this cache provider, ensure that the spring data gemfire jars are on 
 * the classpath of the GemFire server and specify the --spring-xml-location 
 * option from the command line or call {@link Builder#setSpringXmlLocation(String)} 
 * when launching the GemFire server.
 *
 * @author Dan Smith
 * @see ServerLauncherCacheProvider
 * @see SpringContextBootstrappingInitializer
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.
 *      AnnotationConfigApplicationContext
 * @see org.springframework.context.event.ApplicationContextEvent
 * @see org.springframework.context.event.ApplicationEventMulticaster
 * @see org.springframework.context.support.ClassPathXmlApplicationContext
 * @see com.gemstone.gemfire.cache.Declarable
 * @link http://gemfire.docs.pivotal.io/latest/userguide/index.html#basic_config
 *       /the_cache/setting_cache_initializer.html
 */
public class SpringServerLauncherCacheProvider implements ServerLauncherCacheProvider {

	@Override
	public Cache createCache(Properties gemfireProperties, ServerLauncher serverLauncher) {
		if (!serverLauncher.isSpringXmlLocationSpecified()) {
			return null;
		}

		System.setProperty(DistributionConfig.GEMFIRE_PREFIX + DistributionConfig.NAME_NAME,
				serverLauncher.getMemberName());

		createSpringContextBootstrappingInitializer().init(CollectionUtils.createProperties(
				Collections.singletonMap(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
						serverLauncher.getSpringXmlLocation())));

		return SpringContextBootstrappingInitializer.getApplicationContext().getBean(Cache.class);
	}

	/* Used for testing purposes */
	protected SpringContextBootstrappingInitializer createSpringContextBootstrappingInitializer() {
		return new SpringContextBootstrappingInitializer();
	}

}
