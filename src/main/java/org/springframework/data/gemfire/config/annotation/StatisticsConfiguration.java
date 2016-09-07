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

import java.util.Map;
import java.util.Properties;

import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The StatisticsConfiguration class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * that applies additional GemFire/Geode configuration by way of GemFire/Geode System properties to configure
 * GemFire/Geode Statistics.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.EnableStatistics
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class StatisticsConfiguration extends EmbeddedServiceConfigurationSupport {

	public static final boolean DEFAULT_ENABLE_TIME_STATISTICS = false;

	public static final int DEFAULT_ARCHIVE_DISK_SPACE_LIMIT = 0;
	public static final int DEFAULT_ARCHIVE_FILE_SIZE_LIMIT = 0;
	public static final int DEFAULT_STATISTIC_SAMPLE_RATE = 1000;

	/* (non-Javadoc) */
	@Override
	protected Class getAnnotationType() {
		return EnableStatistics.class;
	}

	/* (non-Javadoc) */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		PropertiesBuilder gemfireProperties = new PropertiesBuilder();

		gemfireProperties.setProperty("statistic-sampling-enabled", true);

		gemfireProperties.setPropertyIfNotDefault("archive-disk-space-limit",
			annotationAttributes.get("archiveDiskSpaceLimit"), DEFAULT_ARCHIVE_DISK_SPACE_LIMIT);

		gemfireProperties.setPropertyIfNotDefault("archive-file-size-limit",
			annotationAttributes.get("archiveFileSizeLimit"), DEFAULT_ARCHIVE_FILE_SIZE_LIMIT);

		gemfireProperties.setProperty("enable-time-statistics",
			Boolean.TRUE.equals(annotationAttributes.get("enableTimeStatistics")));

		gemfireProperties.setProperty("statistic-archive-file", annotationAttributes.get("archiveFile"));

		gemfireProperties.setPropertyIfNotDefault("statistic-sample-rate",
			annotationAttributes.get("sampleRate"), DEFAULT_STATISTIC_SAMPLE_RATE);

		return gemfireProperties.build();
	}
}
