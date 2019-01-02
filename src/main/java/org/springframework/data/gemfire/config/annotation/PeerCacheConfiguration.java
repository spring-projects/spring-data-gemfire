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

package org.springframework.data.gemfire.config.annotation;

import java.util.Map;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.CacheFactoryBean;

/**
 * Spring {@link Configuration} class used to configure, construct and initialize
 * a GemFire peer {@link com.gemstone.gemfire.cache.Cache} instance in a Spring application context.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration
 * @see com.gemstone.gemfire.cache.Cache
 * @since 1.9.0
 */
@Configuration
@SuppressWarnings("unused")
public class PeerCacheConfiguration extends AbstractCacheConfiguration {

    protected static final boolean DEFAULT_ENABLE_AUTO_RECONNECT = false;
    protected static final boolean DEFAULT_USE_CLUSTER_CONFIGURATION = false;

    protected static final String DEFAULT_NAME = "SpringBasedPeerCacheApplication";

    private boolean enableAutoReconnect = DEFAULT_ENABLE_AUTO_RECONNECT;
    private boolean useClusterConfiguration = DEFAULT_USE_CLUSTER_CONFIGURATION;

    private Integer lockLease;
    private Integer lockTimeout;
    private Integer messageSyncInterval;
    private Integer searchTimeout;

    @Bean
    public CacheFactoryBean gemfireCache() {
        CacheFactoryBean gemfireCache = constructCacheFactoryBean();

        gemfireCache.setEnableAutoReconnect(enableAutoReconnect());
        gemfireCache.setLockLease(lockLease());
        gemfireCache.setLockTimeout(lockTimeout());
        gemfireCache.setMessageSyncInterval(messageSyncInterval());
        gemfireCache.setSearchTimeout(searchTimeout());
        gemfireCache.setUseBeanFactoryLocator(useBeanFactoryLocator());
        gemfireCache.setUseClusterConfiguration(useClusterConfiguration());

        return gemfireCache;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings("unchecked")
    protected <T extends CacheFactoryBean> T newCacheFactoryBean() {
        return (T) new CacheFactoryBean();
    }

    /**
     * Configures GemFire peer {@link com.gemstone.gemfire.cache.Cache} specific settings.
     *
     * @param importMetadata {@link AnnotationMetadata} containing peer cache meta-data used to configure
     * the GemFire peer {@link com.gemstone.gemfire.cache.Cache}.
     * @see org.springframework.core.type.AnnotationMetadata
     * @see #isCacheServerOrPeerCacheApplication(AnnotationMetadata)
     */
    @Override
    protected void configureCache(AnnotationMetadata importMetadata) {
        super.configureCache(importMetadata);

        if (isCacheServerOrPeerCacheApplication(importMetadata)) {
            Map<String, Object> peerCacheApplicationAttributes =
                importMetadata.getAnnotationAttributes(getAnnotationTypeName());

            setEnableAutoReconnect(Boolean.TRUE.equals(peerCacheApplicationAttributes.get("enableAutoReconnect")));
            setLockLease((Integer) peerCacheApplicationAttributes.get("lockLease"));
            setLockTimeout((Integer) peerCacheApplicationAttributes.get("lockTimeout"));
            setMessageSyncInterval((Integer) peerCacheApplicationAttributes.get("messageSyncInterval"));
            setSearchTimeout((Integer) peerCacheApplicationAttributes.get("searchTimeout"));
            setUseClusterConfiguration(Boolean.TRUE.equals(peerCacheApplicationAttributes.get("useClusterConfiguration")));

            String locators = (String) peerCacheApplicationAttributes.get("locators");

            if (hasValue(locators)) {
                setLocators(locators);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class getAnnotationType() {
        return PeerCacheApplication.class;
    }

    /* (non-Javadoc) */
    void setEnableAutoReconnect(boolean enableAutoReconnect) {
        this.enableAutoReconnect = enableAutoReconnect;
    }

    protected boolean enableAutoReconnect() {
        return this.enableAutoReconnect;
    }

    /* (non-Javadoc) */
    void setLockLease(Integer lockLease) {
        this.lockLease = lockLease;
    }

    protected Integer lockLease() {
        return this.lockLease;
    }

    /* (non-Javadoc) */
    void setLockTimeout(Integer lockTimeout) {
        this.lockTimeout = lockTimeout;
    }

    protected Integer lockTimeout() {
        return this.lockTimeout;
    }

    /* (non-Javadoc) */
    void setMessageSyncInterval(Integer messageSyncInterval) {
        this.messageSyncInterval = messageSyncInterval;
    }

    protected Integer messageSyncInterval() {
        return this.messageSyncInterval;
    }

    /* (non-Javadoc) */
    void setSearchTimeout(Integer searchTimeout) {
        this.searchTimeout = searchTimeout;
    }

    protected Integer searchTimeout() {
        return this.searchTimeout;
    }

    /* (non-Javadoc) */
    void setUseClusterConfiguration(boolean useClusterConfiguration) {
        this.useClusterConfiguration = useClusterConfiguration;
    }

    protected boolean useClusterConfiguration() {
        return this.useClusterConfiguration;
    }

    @Override
    public String toString() {
        return DEFAULT_NAME;
    }
}
