/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.CacheUtils.isClient;

import java.lang.annotation.Annotation;
import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.Index;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.context.event.ApplicationContextEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.OrderComparator;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.admin.remote.FunctionGemfireAdminTemplate;
import org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.config.schema.SchemaObjectCollector;
import org.springframework.data.gemfire.config.schema.SchemaObjectDefiner;
import org.springframework.data.gemfire.config.schema.definitions.RegionDefinition;
import org.springframework.data.gemfire.config.schema.support.ClientRegionCollector;
import org.springframework.data.gemfire.config.schema.support.ComposableSchemaObjectCollector;
import org.springframework.data.gemfire.config.schema.support.ComposableSchemaObjectDefiner;
import org.springframework.data.gemfire.config.schema.support.IndexCollector;
import org.springframework.data.gemfire.config.schema.support.IndexDefiner;
import org.springframework.data.gemfire.config.schema.support.RegionDefiner;

/**
 * Spring {@link Configuration @Configuration} class defining Spring beans that will record the creation of
 * Apache Geode / Pivotal GemFire {@link Region Regions} defined in Spring config (i.e. XML, Java or by Annotations)
 * as Spring beans in the Spring container.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.context.event.EventListener
 * @since 2.0.0
 */
@Configuration
@SuppressWarnings("unused")
public class ClusterConfigurationConfiguration extends AbstractAnnotationConfigSupport implements ImportAware {

	protected static final boolean DEFAULT_MANAGEMENT_USE_HTTP = false;

	protected static final int DEFAULT_MANAGEMENT_HTTP_PORT = HttpServiceConfiguration.DEFAULT_HTTP_SERVICE_PORT;

	protected static final String DEFAULT_MANAGEMENT_HTTP_HOST = "localhost";

	private static final RegionShortcut DEFAULT_SERVER_REGION_SHORTCUT = RegionDefinition.DEFAULT_REGION_SHORTCUT;

	private Boolean useHttp = DEFAULT_MANAGEMENT_USE_HTTP;

	private Integer managementHttpPort = DEFAULT_MANAGEMENT_HTTP_PORT;

	private RegionShortcut serverRegionShortcut;

	private String managementHttpHost = DEFAULT_MANAGEMENT_HTTP_HOST;

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableClusterConfiguration.class;
	}

	protected void setManagementHttpHost(String hostname) {
		this.managementHttpHost = hostname;
	}

	protected Optional<String> getManagementHttpHost() {
		return Optional.ofNullable(this.managementHttpHost);
	}

	protected String resolveManagementHttpHost() {
		return getManagementHttpHost().orElse(DEFAULT_MANAGEMENT_HTTP_HOST);
	}

	protected void setManagementHttpPort(Integer managementHttpPort) {
		this.managementHttpPort = managementHttpPort;
	}

	protected Optional<Integer> getManagementHttpPort() {
		return Optional.ofNullable(this.managementHttpPort);
	}

	protected int resolveManagementHttpPort() {
		return getManagementHttpPort().orElse(DEFAULT_MANAGEMENT_HTTP_PORT);
	}

	protected void setManagementUseHttp(Boolean useHttp) {
		this.useHttp = useHttp;
	}

	protected Optional<Boolean> getManagementUseHttp() {
		return Optional.ofNullable(this.useHttp);
	}

	protected boolean resolveManagementUseHttp() {
		return getManagementUseHttp().orElse(DEFAULT_MANAGEMENT_USE_HTTP);
	}

	protected void setServerRegionShortcut(RegionShortcut regionShortcut) {
		this.serverRegionShortcut = regionShortcut;
	}

	protected Optional<RegionShortcut> getServerRegionShortcut() {
		return Optional.ofNullable(this.serverRegionShortcut);
	}

	protected RegionShortcut resolveServerRegionShortcut() {
		return getServerRegionShortcut().orElse(DEFAULT_SERVER_REGION_SHORTCUT);
	}

	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {

		if (isAnnotationPresent(importMetadata)) {

			AnnotationAttributes enableClusterConfigurationAttributes = getAnnotationAttributes(importMetadata);

			setManagementHttpHost(resolveProperty(managementProperty("http.hostname"),
				DEFAULT_MANAGEMENT_HTTP_HOST));

			setManagementHttpPort(resolveProperty(managementProperty("http.port"),
				DEFAULT_MANAGEMENT_HTTP_PORT));

			setManagementUseHttp(resolveProperty(managementProperty("use-http"),
				DEFAULT_MANAGEMENT_USE_HTTP));

			setServerRegionShortcut(resolveProperty(clusterProperty("region.type"),
				RegionShortcut.class, DEFAULT_SERVER_REGION_SHORTCUT));
		}
	}

	@EventListener
	public void gemfireClusterSchemaCreationHandler(ContextRefreshedEvent event) {

		GemFireCache gemfireCache = resolveGemFireCache(event);

		if (isClient(gemfireCache)) {

			GemfireAdminOperations gemfireAdminOperations = newGemfireAdminOperations((ClientCache) gemfireCache);

			SchemaObjectDefiner schemaObjectDefiner = newSchemaObjectDefiner();

			Iterable<?> schemaObjects = newSchemaObjectCollector().collectFrom(resolveApplicationContext(event));

			stream(schemaObjects.spliterator(), false)
				.map(schemaObjectDefiner::define)
				.sorted(OrderComparator.INSTANCE)
				.forEach(schemaObjectDefinition ->
					schemaObjectDefinition.ifPresent(it -> it.create(gemfireAdminOperations)));
		}
		/*
		else if (isPeer(gemfireCache)) {

			GemfireFunctionUtils.registerFunctionForPojoMethod(new CreateRegionFunction(),
				CreateRegionFunction.CREATE_REGION_FUNCTION_ID);

			GemfireFunctionUtils.registerFunctionForPojoMethod(new CreateIndexFunction(),
				CreateIndexFunction.CREATE_INDEX_FUNCTION_ID);
		}
		*/
	}

	/**
	 * Resolves a reference to the Spring {@link ApplicationContext} from the given {@link ApplicationContextEvent}.
	 *
	 * @param event {@link ApplicationContextEvent} from which to resolve the Spring {@link ApplicationContext}.
	 * @return the resolved Spring {@link ApplicationContext}.
	 * @see org.springframework.context.event.ApplicationContextEvent
	 * @see org.springframework.context.ApplicationContext
	 */
	private ApplicationContext resolveApplicationContext(ApplicationContextEvent event) {
		return event.getApplicationContext();
	}

	/**
	 * Tries to resolve the {@link GemFireCache} from the Spring {@link ApplicationContext}.
	 * The {@link GemFireCache} will be resolvable from the Spring {@link ApplicationContext} if the cache
	 * was registered a managed bean in the Spring container.
	 *
	 * If the {@link GemFireCache} cannot be resolved from the {@link ApplicationContext}, this method will attempt
	 * to resolve the cache reference from GemFire's global context using the GemFire API.
	 *
	 * @param event Spring {@link ApplicationContextEvent} encapsulating the details of the Spring container event.
	 * @return the resolved {@link GemFireCache} if available.
	 * @see org.springframework.context.event.ApplicationContextEvent
	 * @see org.apache.geode.cache.GemFireCache
	 */
	private GemFireCache resolveGemFireCache(ApplicationContextEvent event) {

		try {
			return resolveApplicationContext(event).getBean(GemFireCache.class);
		}
		catch (BeansException ignore) {
			return GemfireUtils.resolveGemFireCache();
		}
	}

	/**
	 * Constructs an instance of {@link GemfireAdminOperations} to perform administrative, schema functions
	 * on a GemFire cache cluster as well as a client cache from a cache client.
	 *
	 * @param clientCache {@link ClientCache} instance used by the {@link GemfireAdminOperations} interface
	 * to access the GemFire system.
	 * @return an implementation of the {@link GemfireAdminOperations} interface to perform administrative functions
	 * on a GemFire system.
	 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
	 * @see org.apache.geode.cache.client.ClientCache
	 */
	private GemfireAdminOperations newGemfireAdminOperations(ClientCache clientCache) {

		if (resolveManagementUseHttp()) {

			String host = resolveManagementHttpHost();
			int port = resolveManagementHttpPort();

			return new RestHttpGemfireAdminTemplate(clientCache, host, port);
		}
		else {
			return new FunctionGemfireAdminTemplate(clientCache);
		}
	}

	/**
	 * Constructs an instance of {@link SchemaObjectCollector} to inspect the application's context
	 * and find all the GemFire schema objects declared of a particular type or types.
	 *
	 * @return a new instance of {@link SchemaObjectCollector} to inspect a GemFire system schema
	 * in search of specific GemFire schema objects (e.g. {@link Region} or {@link Index}).
	 * @see org.springframework.data.gemfire.config.schema.SchemaObjectCollector
	 */
	private SchemaObjectCollector<?> newSchemaObjectCollector() {

		return ComposableSchemaObjectCollector.compose(
			new ClientRegionCollector(),
			new IndexCollector()
		);
	}

	/**
	 * Constructs an instance of {@link SchemaObjectDefiner} used to reverse engineer a GemFire schema object instance
	 * to build a definition.
	 *
	 * @return a new instance of {@link SchemaObjectDefiner}.
	 * @see org.springframework.data.gemfire.config.schema.SchemaObjectDefiner
	 */
	private SchemaObjectDefiner newSchemaObjectDefiner() {

		return ComposableSchemaObjectDefiner.compose(
			new RegionDefiner(resolveServerRegionShortcut()),
			new IndexDefiner()
		);
	}
}
