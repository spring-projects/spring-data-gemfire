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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.data.gemfire.util.PropertiesBuilder;
import org.springframework.util.StringUtils;

/**
 * The {@link SslConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} that applies
 * additional configuration using Pivotal GemFire/Apache Geode {@link Properties} to configure SSL.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.EnableSsl
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class SslConfiguration extends EmbeddedServiceConfigurationSupport {

	/**
	 * Returns the {@link EnableSsl} {@link Annotation} {@link Class type}.
	 *
	 * @return the {@link EnableSsl} {@link Annotation} {@link Class type}.
	 * @see org.springframework.data.gemfire.config.annotation.EnableSsl
	 */
	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableSsl.class;
	}

	/* (non-Javadoc) */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributesMap) {

		AnnotationAttributes annotationAttributes = AnnotationAttributes.fromMap(annotationAttributesMap);

		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		Set<EnableSsl.Component> components = resolveComponents(annotationAttributes);

		gemfireProperties.setProperty("ssl-enabled-components",
			StringUtils.collectionToCommaDelimitedString(components.stream()
				.map(EnableSsl.Component::toString)
				.collect(Collectors.toSet())))

			.setProperty("ssl-ciphers", resolveProperty(sslProperty("ciphers"),
				StringUtils.arrayToCommaDelimitedString(annotationAttributes.getStringArray("ciphers"))))

			.setPropertyIfNotDefault("ssl-default-alias",
				resolveProperty(sslProperty("certificate.alias.default"),
					annotationAttributes.getString("defaultCertificateAlias")), "")

			.setProperty("ssl-keystore", resolveProperty(sslProperty("keystore"),
				annotationAttributes.getString("keystore")))

			.setProperty("ssl-keystore-password", resolveProperty(sslProperty("keystore.password"),
				annotationAttributes.getString("keystorePassword")))

			.setProperty("ssl-keystore-type", resolveProperty(sslProperty("keystore.type"),
				annotationAttributes.getString("keystoreType")))

			.setProperty("ssl-protocols", resolveProperty(sslProperty("protocols"),
				StringUtils.arrayToCommaDelimitedString(annotationAttributes.getStringArray("protocols"))))

			.setProperty("ssl-require-authentication",
				resolveProperty(sslProperty("require-authentication"),
					annotationAttributes.getBoolean("requireAuthentication")))

			.setProperty("ssl-truststore", resolveProperty(sslProperty("truststore"),
				annotationAttributes.getString("truststore")))

			.setProperty("ssl-truststore-password", resolveProperty(sslProperty("truststore.password"),
				annotationAttributes.getString("truststorePassword")))

			.setProperty("ssl-web-require-authentication",
				resolveProperty(sslProperty("web-require-authentication"),
					annotationAttributes.getBoolean("webRequireAuthentication")));

		configureComponentCertificateAliases(annotationAttributes, gemfireProperties);

		return gemfireProperties.build();
	}

	private Set<EnableSsl.Component> resolveComponents(AnnotationAttributes annotationAttributes) {

		Set<EnableSsl.Component> components =
			Arrays.stream(Optional.of(resolveProperty(sslProperty("components"), ""))
				.filter(StringUtils::hasText)
				.map(StringUtils::commaDelimitedListToStringArray)
				.orElseGet(() -> new String[0]))
				.map(EnableSsl.Component::valueOfName)
				.collect(Collectors.toSet());

		components = components.isEmpty()
			? CollectionUtils.asSet((EnableSsl.Component[]) annotationAttributes.get("components"))
			: components;

		components = components.isEmpty() ? Collections.singleton(EnableSsl.Component.ALL) : components;

		return components;
	}

	private void configureComponentCertificateAliases(AnnotationAttributes annotationAttributes,
			PropertiesBuilder gemfireProperties) {

		AnnotationAttributes[] componentCertificateAliases =
			annotationAttributes.getAnnotationArray("componentCertificateAliases");

		Arrays.stream(componentCertificateAliases).forEach(aliasAttributes -> {

			EnableSsl.Component component = aliasAttributes.getEnum("component");
			String alias = aliasAttributes.getString("alias");

			gemfireProperties.setProperty(String.format("ssl-%s-alias", component), alias);
		});

		Arrays.stream(EnableSsl.Component.values()).forEach(component -> {

			String propertyNameSuffix = String.format("certificate.alias.%s", component);

			Optional.ofNullable(resolveProperty(sslProperty(propertyNameSuffix), ""))
				.filter(StringUtils::hasText)
				.ifPresent(alias ->
					gemfireProperties.setProperty(String.format("ssl-%s-alias", component),
						StringUtils.trimWhitespace(alias)));
		});
	}
}
